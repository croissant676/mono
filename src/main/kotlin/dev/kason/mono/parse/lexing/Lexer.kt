package dev.kason.mono.parse.lexing

import dev.kason.mono.core.*

// indicates that the given character is outside the range of the cursor.
const val OUT_OF_RANGE: Char = 0.toChar()

class LexerCursor(val range: IndexRange) : Iterator<Char> {
	// if position is at a given location, i, source[i] is the next char to be read
	var position: Int = range.startIndex
	val current: Char get() = current()
	val next: Char get() = current(1)
	fun outOfBounds(offset: Int): Boolean {
		val location = position + offset
		return location < range.startIndex || location > range.endIndex
	}

	fun current(offset: Int = 0): Char {
		if (outOfBounds(offset)) return OUT_OF_RANGE
		return range.document[position + offset]
	}

	fun eat(): Char {
		val char = current()
		position++
		return char
	}

	// returns the character that breaks that predicate, or
	// the out of range character if it reached the end.
	inline fun eatWhile(predicate: Char.() -> Boolean): Char {
		while (hasNext() && predicate(current())) next()
		return current()
	}

	val positionIndex: Index
		get() = range.document.index(position)

	override fun hasNext(): Boolean = !outOfBounds(0)
	override fun next(): Char = eat()
}

// char types
fun Char.isIdentifierStart(): Boolean = isLetter() || this == '_'
fun Char.isIdentifierPart(): Boolean = isLetterOrDigit() || this == '_'
fun Char.isQuoteIdentifierPart(): Boolean =
	this != '`' && this != '\n' && this != OUT_OF_RANGE

fun Char.isOtherWhitespace(): Boolean =
	this.isWhitespace() && this != '\n' && this != '\t'

fun Char.isDigit(): Boolean = this in '0'..'9'
fun Char.isHexDigit(): Boolean = isDigit() || (this in 'a'..'f') || (this in 'A'..'F')

class Lexer(val range: IndexRange) : Iterable<Token> {
	constructor(document: Document) : this(document.fullRange)

	val cursor = LexerCursor(range)
	private val tokenBacklog: MutableList<Token> = mutableListOf()
	val document: Document get() = range.document

	internal fun eatIdentifier() {
		check(cursor.current.isIdentifierStart())
		cursor.eatWhile { isIdentifierPart() }
	}

	internal fun eatQuotedIdentifier() {
		check(cursor.current == '`')
		cursor.eat()
		cursor.eatWhile { isQuoteIdentifierPart() }
		if (cursor.next != '`') {
			error("quoted identifier did not end with `")
		}
		cursor.eat()
	}

	internal fun eatComment(): CommentTokenKind {
		check(cursor.current == '#')
		if (cursor.next == '[' || cursor.next == '(') {
			return eatBlockComment()
		}
		return eatLineComment()
	}

	internal fun eatBlockComment(): CommentTokenKind {
		val nesting: ArrayDeque<Char> = ArrayDeque()
		cursor.eat() // we eat the first #, now it's either [ or (
		val element = cursor.eat()
		val isDocComment = element == '['
		nesting.add(element)
		while (cursor.hasNext()) {
			val char = cursor.current
			if (char == ')' || char == ']' && cursor.next == '#') {
				val last = nesting.last()
				if ((last == '(' && char == ')') || (last == '[' && char == ']')) {
					cursor.eat()
					cursor.eat()
					nesting.removeLast()
					if (nesting.isEmpty()) break
				}
			} else if (char == '#') {
				val nextChar = cursor.next
				if (nextChar == '[' || nextChar == '(') {
					cursor.eat()
					nesting.addLast(cursor.eat())
				}
			} else cursor.eat()
		}
		return if (isDocComment) CommentTokenKind.DocBlock else CommentTokenKind.Block
	}

	internal fun eatLineComment(): CommentTokenKind {
		val isDocComment = cursor.next == '#'
		cursor.eatWhile { this != '\n' }
		return if (isDocComment) CommentTokenKind.DocLine else CommentTokenKind.Line
	}

	internal fun eatNumber(): NumberLiteralTokenKind {
		check(cursor.current.isDigit())
		var base = Base.Dec
		if (cursor.eat() == '0') {
			when (cursor.current) {
				'b', 'o' -> {
					base = Base[cursor.eat()]!!
					if (!eatDecimalDigits()) {
						return IntLiteralTokenKind(base, isEmpty = true)
					}
				}

				'x' -> {
					base = Base.Hex
					if (!eatHexDigits()) {
						return IntLiteralTokenKind(base, isEmpty = true)
					}
				}
				in '0'..'9', '_' -> eatDecimalDigits()
				'.', 'e', 'E' -> {} // move on to next step
				else -> return IntLiteralTokenKind(base, isEmpty = false) // just 0
			}
		} else {
			eatDecimalDigits()
		}

		val current = cursor.current
		val next = cursor.next
		if (current == '.' && next != '.' && !next.isIdentifierStart()) {
			// we don't want a range function
			// or a member ref, ie '55.toString()'
			cursor.eat()
			var emptyExponent = false
			if (next.isDigit()) { // we read after decimal point
				eatDecimalDigits()
				if (cursor.current == 'e' || cursor.current == 'E') {
					emptyExponent = !eatExponent()
				}
			}
			return FloatLiteralTokenKind(base, emptyExponent)
		} else if (current == 'e' || current == 'E') {
			val emptyExponent = !eatExponent()
			return FloatLiteralTokenKind(base, emptyExponent)
		}
		return IntLiteralTokenKind(base, false)
	}

	// returns whether anything of value as been read
	private fun eatDecimalDigits(): Boolean {
		var readAnything = false
		cursor.eatWhile {
			if (isDigit()) {
				readAnything = true
				true
			} else this == '_'
		}
		return readAnything
	}

	private fun eatHexDigits(): Boolean {
		var readAnything = false
		cursor.eatWhile {
			if (isHexDigit()) {
				readAnything = true
				true
			} else this == '_'
		}
		return readAnything
	}

	private fun eatExponent(): Boolean {
		cursor.eat() // read the e/E
		if (cursor.current == '+' || cursor.current == '-') cursor.eat()
		return eatDecimalDigits()
	}

	internal fun eatCharLiteral() {
		check(cursor.current == '\'')
		cursor.eat()
		if (cursor.next == '\'' && cursor.current != '\\') {
			// basic char, like 'h'
			cursor.eat()
			cursor.eat()
			return
		}
		while (cursor.hasNext()) {
			val current = cursor.current
			if (current == '\'') {
				cursor.eat()
				return
			} else if (current == '\n' && cursor.next != '\\') {
				break
			} else if (current == '\\') {
				cursor.eat()
				cursor.eat() // skip next char
			} else {
				cursor.eat()
			}
		}
		error("char literal ended unexpectedly")
	}

	// the locations of all the interpolations for that moment
	// for instance "${ "${ 5 }" }"
	// 	 			  ^   ^ | <- cursor
	//               index locations
	// if empty, all interpolation has been resolved
	private val interpolationLocations: MutableList<Index> = mutableListOf()

	// the nesting level within the interpolation
	// ${ {} }
	//    ^ contributes 1
	// only when braceNesting is 0 can the interpolation exit
	private var braceNesting = 0

	internal fun eatStringLiteral() {
		// allow } because  interpolation
		check(cursor.current == '"' || cursor.current == '}')
		cursor.eat()
		while (cursor.hasNext()) {
			val current = cursor.current
			if (current == '"') {
				cursor.eat()
				return
			} else if (current == '$') {
				cursor.eat()
				// dont check for bounds
				// because if $ is last, then we will add $ to string
				// leading to break and error
				val next = cursor.current
				if (next == '{') {
					interpolationLocations.add(cursor.positionIndex)
					cursor.eat()
					return
				} else if (next == '`') {
					tokenBacklog.add(
						createToken(TokenKinds.Identifier) { eatQuotedIdentifier() }
					)
				} else if (next.isIdentifierStart()) {
					tokenBacklog.add(
						createToken(TokenKinds.Identifier) { eatIdentifier() }
					)
				} else continue
			} else if (current == '\\') {
				if (cursor.current == '"' || cursor.current == '\$') {
					cursor.eat()
				}
				cursor.eat()
			} else cursor.eat()
		}
		error("str literal ended unexpectedly")
	}

	internal fun eatCurlyBrackets(): TokenKind {
		val current = cursor.current
		check(current == '{' || current == '}')
		if (current == '{') {
			cursor.eat()
			if (interpolationLocations.isNotEmpty()) braceNesting++ // we're in an interpolation
			return TokenKinds.Symbol
		}
		if (interpolationLocations.isNotEmpty()) {
			if (braceNesting == 0) {
				interpolationLocations.removeLast()
				eatStringLiteral()
				return LiteralTokenKinds.String
			}
			braceNesting--
		}
		cursor.eat()
		return TokenKinds.Symbol
	}

	internal fun readWhitespace() = cursor.eatWhile { isOtherWhitespace() }
	internal val singleCharacterTokens = ";,.()[]@~?:$=!<>+-*%^&|/\\".toSet()

	fun nextToken(): Token {
		val peek = cursor.current()
		if (peek == OUT_OF_RANGE) return Token(TokenKinds.EOF, range)
		if (tokenBacklog.isNotEmpty()) return tokenBacklog.removeFirst()
		return when {
			peek == '#' -> {
				createToken { eatComment() }
			}

			peek == '`' -> createToken(TokenKinds.Identifier) { eatQuotedIdentifier() }
			peek == '\n' -> createToken(TokenKinds.Newline) { cursor.eat() }
			peek == '\t' -> createToken(TokenKinds.Indent) { cursor.eat() }
			peek.isIdentifierStart() -> {
				val start = cursor.positionIndex
				eatIdentifier()
				val indexRange = start..<cursor.position
				val text = indexRange.read()
				if (text.toBooleanStrictOrNull() != null) {
					Token(LiteralTokenKinds.Boolean, indexRange)
				}
				Token(TokenKinds.Identifier, indexRange)
			}

			peek.isDigit() -> createToken { eatNumber() }
			peek == '\'' -> createToken(LiteralTokenKinds.Char) { eatCharLiteral() }
			peek == '\"' -> createToken(LiteralTokenKinds.String) { eatStringLiteral() }
			peek == '{' || peek == '}' -> createToken { eatCurlyBrackets() }
			peek.isOtherWhitespace() -> createToken(TokenKinds.Whitespace) { readWhitespace() }
			peek in singleCharacterTokens -> createToken(TokenKinds.Symbol) { cursor.eat() }
			else -> createToken(TokenKinds.Unknown) { cursor.eat() }
		}
	}

	override fun iterator(): Iterator<Token> = object : Iterator<Token> {
		override fun hasNext(): Boolean = cursor.hasNext()
		override fun next(): Token = nextToken()
	}

	internal fun createToken(tokenKind: TokenKind, block: () -> Unit): Token {
		val start = cursor.positionIndex
		block()
		return Token(tokenKind, start..<cursor.position)
	}

	internal fun createToken(block: () -> TokenKind): Token {
		val start = cursor.positionIndex
		val kind = block()
		return Token(kind, start..<cursor.position)
	}

}

fun TokenStream(lexer: Lexer): TokenStream {
	val tokens: MutableList<Token> = mutableListOf()
	for (token in lexer) {
		tokens.add(token)
	}
	return TokenStream(lexer.document, tokens)
}
