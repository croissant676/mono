package dev.kason.mono.parse.lexing

import dev.kason.mono.core.Document
import dev.kason.mono.core.Index
import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.read

class TokenStream(val document: Document, val tokens: List<Token>) : List<Token> by tokens {

	fun postOperations(): TokenStream = removeNLAfterBackslash()
		.addPseudoIndentations()
		.pairIndent()
		.glueSymbols()

	override fun toString(): String {
		val builder = StringBuilder()
		for (token in tokens) {
			builder.append(token)
			builder.append('\n')
		}
		return builder.toString()
	}
}

class TokenCursor(val tokenStream: TokenStream) : Iterator<Token> {
	var position: Int = 0
		private set
	val reachedEnd: Boolean get() = position >= tokenStream.size
	val current: Token? get() = peek()
	val next: Token? get() = peek(1)
	val previous: Token? get() = peek(-1)
	fun outOfBounds(offset: Int): Boolean = position + offset in tokenStream.indices

	fun peek(offset: Int = 0): Token? = tokenStream.getOrNull(position + offset)
	fun eat(): Token? {
		val token = peek()
		if (token != null) {
			position++
		}
		return token
	}

	override fun hasNext(): Boolean = !outOfBounds(0)
	override fun next(): Token = eat() ?: throw NoSuchElementException()

	inline fun eatWhile(predicate: TokenPredicate): List<Token> {
		val list: MutableList<Token> = mutableListOf()
		var token: Token?
		do {
			token = eat()
			if (token != null && token satisfies predicate) {
				list.add(token)
			} else break
		} while (true)
		return list
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
				}
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
		if (cursor.next?.tokenKind != TokenKinds.Newline) {
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


private val actionMap = mapOf(
	"<" to "<=",
	"<<" to "=",
	">" to ">=",
	">>" to "=",
	">>>" to "=",
	"!" to "=",
	"=" to "=",
	"+" to "+=",
	"-" to "-=",
	"*" to "*=",
	"/" to "=",
	"**" to "=",
	"%" to "=",
	"|" to "|=",
	"&" to "&=",
	"^" to "=",
	"~" to "=",
	"?" to ":"
)

fun TokenStream.glueSymbols(): TokenStream {
	val cursor = TokenCursor(this)
	val newStream: MutableList<Token> = mutableListOf()
	var start: Index? = null
	fun rangeUpToCurrentToken(): IndexRange {
		val properStart = start!!
		val properEnd = cursor.previous!!.range.endInclusive
		return properStart..properEnd
	}
	for (token in cursor) {
		if (token.tokenKind != TokenKinds.Symbol) {
			if (start != null) {
				newStream += Token(TokenKinds.Symbol, rangeUpToCurrentToken())
				start = null
			}
			newStream += token
		} else if (start == null) {
			start = token.range.start
		} else {
			val currentRange = rangeUpToCurrentToken()
			if (currentRange.read() in actionMap.keys && token.text in actionMap[currentRange.read()]!!) {
				continue
			} else {
				newStream += Token(TokenKinds.Symbol, currentRange)
				start = token.range.start
			}
		}
	}
	return TokenStream(document, newStream)
}
