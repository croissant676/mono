package dev.kason.mono.parse.lexing

import dev.kason.mono.core.Document
import dev.kason.mono.core.Index
import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.read
import javax.swing.plaf.basic.BasicSliderUI.ActionScroller

class TokenStream(val document: Document, val tokens: List<Token>) : List<Token> by tokens {

	fun postOperations(): TokenStream = removeNLAfterBackslash()
		.addPseudoIndentations()
		.pairIndent()
		.glueSymbols()
		.glueFunctionalPairs()
		.addHardKeywords()

	override fun toString(): String {
		val builder = StringBuilder()
		for (token in tokens) {
			builder.append(token)
			builder.append('\n')
		}
		return builder.toString()
	}
}

fun TokenStream.filter(predicate: (Token) -> Boolean): TokenStream =
	TokenStream(document, tokens.filter(predicate))

fun TokenStream.removeUnnecessaryTokens(): TokenStream =
	filter {
		it.tokenKind != TokenKinds.Whitespace && it.tokenKind != TokenKinds.Unknown
			&& it.tokenKind !is CommentTokenKind
	}

class TokenCursor(val tokenStream: TokenStream) : Iterator<Token> {
	var position: Int = 0
		private set

	val current: Token? get() = peek()
	val next: Token? get() = peek(1)
	val previous: Token? get() = peek(-1)
	fun outOfBounds(offset: Int = 0): Boolean = (position + offset) !in tokenStream.indices

	fun peek(offset: Int = 0): Token? = tokenStream.getOrNull(position + offset)
	fun eat(): Token? {
		val token = current
		if (token != null) {
			position++
		}
		return token
	}

	fun expect(predicate: (Token) -> Boolean): Token {
		val token = eat()
		if (token == null || !predicate(token)) {
			throw NoSuchElementException("current token $token does not satisfy predicate")
		}
		return token
	}

	override fun hasNext(): Boolean = !outOfBounds()
	override fun next(): Token = eat() ?: throw NoSuchElementException("position $position")

	fun next(number: Int): List<Token> {
		val list: MutableList<Token> = mutableListOf()
		repeat(number) {
			list += eat() ?: throw NoSuchElementException("position $position")
		}
		return list
	}

	fun eatIf(predicate: (Token) -> Boolean): Token? {
		val token = peek()
		if (token != null && predicate(token)) {
			eat()
			return token
		}
		return null
	}

	inline fun eatWhile(predicate: (Token) -> Boolean): List<Token> {
		val list: MutableList<Token> = mutableListOf()
		var token: Token?
		do {
			token = peek()
			if (token != null && predicate(token)) {
				list.add(token)
				eat()
			} else break
		} while (true)
		return list
	}

	// returns the result of the block if it succeeds, if not
	// it will return null and reset the position
	fun <T> transaction(block: TokenCursor.() -> T): T? {
		val oldPosition = position
		return try {
			block()
		} catch (e: Exception) {
			position = oldPosition
			e.printStackTrace()
			null
		}
	}
}

// removes the lexer indent tokens and creates pseudo indent dedent tokens
fun TokenStream.addPseudoIndentations(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	var previousLineIndent = 0
	while (cursor.hasNext()) {
		val line = cursor.eatWhile { it.tokenKind != TokenKinds.Newline }
		val indent = line.takeWhile { it.tokenKind == TokenKinds.Indent }.size
		val difference = indent - previousLineIndent
		if (difference < 0) {
			repeat(-difference) {
				newStream += Token(TokenKinds.Dedent, document.emptyRange)
			}
		} else if (difference > 0) {
			repeat(difference) {
				newStream += Token(TokenKinds.Indent, document.emptyRange)
			}
		}
		previousLineIndent = indent
		newStream += line.drop(indent)
		newStream += cursor.eat() ?: break
	}
	repeat(previousLineIndent) {
		newStream += Token(TokenKinds.Dedent, document.emptyRange)
	}
	return TokenStream(document, newStream)
}

// checks if there are indent tokens right next to dedent tokens and cancels them out
fun TokenStream.pairIndent(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	while (cursor.hasNext()) {
		val token = cursor.eat() ?: break
		when (token.tokenKind) {
			TokenKinds.Indent, TokenKinds.Dedent -> {
				val tokens = cursor.eatWhile {
					it.tokenKind == TokenKinds.Indent || it.tokenKind == TokenKinds.Dedent
				} + token
				var netIndent = 0
				tokens.forEach {
					if (it.tokenKind == TokenKinds.Indent) netIndent++
					else netIndent--
				}
				if (netIndent > 0) {
					repeat(netIndent) {
						newStream += Token(TokenKinds.Indent, document.emptyRange)
					}
				} else if (netIndent < 0) {
					repeat(-netIndent) {
						newStream += Token(TokenKinds.Dedent, document.emptyRange)
					}
				}
			}

			else -> newStream += token
		}
	}
	return TokenStream(document, newStream)
}

fun TokenStream.removeNLAfterBackslash(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	while (cursor.hasNext()) {
		val token = cursor.eat() ?: break
		if (token.text != "\\") {
			newStream += token
			continue
		}
		val tokens: MutableList<Token> = mutableListOf(token) // tokens we add back if we can't find a newline
		tokens += cursor.eatWhile { it.tokenKind == TokenKinds.Whitespace }
		if (cursor.current?.tokenKind != TokenKinds.Newline) {
			newStream += tokens
			continue
		}
		cursor.eat() // consume newline
		// get rid of any indentation tokens after the newline
		cursor.eatWhile { it.tokenKind == TokenKinds.Indent || it.tokenKind == TokenKinds.Dedent }
		// theoretically, no dedents should be here, but just in case
	}
	return TokenStream(document, newStream)
}


@OptIn(ExperimentalStdlibApi::class)
private fun createActionMap(vararg actions: String) = buildMap<String, String> {
	for (action in actions) {
		for (characterCount in 1..<action.length) {
			val key = action.substring(0, characterCount)
			val value = action[characterCount]
			merge(key, value.toString(), String::plus)
		}
	}
}

private val actionMap = createActionMap(
	"<-",
	"<=",
	">=",
	"==", "!=",
	"===", "!==",
	"->", "=>",
	"++", "--",
	"**", "+=", "-=", "*=", "/=", "%=",
	"?:", "..",
)

fun TokenStream.glueSymbols(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	var start: Index? = null
	fun rangeUpTo(token: Token = cursor.peek(-2)!!): IndexRange {
		val properStart = start!!
		val properEnd = token.range.endInclusive
		return properStart..properEnd
	}
	for (token in cursor) {
		if (token.tokenKind != TokenKinds.Symbol) {
			if (start != null) {
				newStream += Token(TokenKinds.Symbol, rangeUpTo())
				start = null
			}
			newStream += token
		} else if (start == null) {
			start = token.range.start
		} else {
			val currentRange = rangeUpTo()
			if (currentRange.read() in actionMap.keys && token.text in actionMap[currentRange.read()]!!) {
				continue
			} else {
				newStream += Token(TokenKinds.Symbol, currentRange)
				start = token.range.start
			}
		}
	}
	if (start != null) {
		newStream += Token(TokenKinds.Symbol, rangeUpTo(cursor.previous!!))
	}
	return TokenStream(document, newStream)
}

fun TokenStream.gluePair(first: String, second: String): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	for (token in cursor) {
		if (token.text != first) {
			newStream += token
			continue
		}
		val next = cursor.peek()
		if (next?.text == second) {
			newStream += Token(TokenKinds.Keyword, token.range.start..next.range.endInclusive)
			cursor.eat()
		} else {
			newStream += token
		}
	}
	return TokenStream(document, newStream)
}

private val functionalPairs = listOf(
	"as" to "?",
	"!" to "is",
	"!" to "in",
)

fun TokenStream.glueFunctionalPairs(): TokenStream =
	functionalPairs.fold(this) { stream, (first, second) ->
		stream.gluePair(first, second)
	}

val hardKeywords = listOf(
	"as",
	"as?",
	"break",
	"struct",
	"continue",
	"do",
	"else",
	"for",
	"def",
	"if",
	"in",
	"!in",
	"is",
	"!is",
	"return",
	"impl",
	"self",
	"throw",
	"try",
	"while",
	"mut",
	"var"
)

fun TokenStream.addHardKeywords(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	for (token in cursor) {
		if (token.tokenKind != TokenKinds.Identifier) {
			newStream += token
			continue
		}
		newStream += if (token.text in hardKeywords) {
			Token(TokenKinds.Keyword, token.range)
		} else {
			token
		}
	}
	return TokenStream(document, newStream)
}