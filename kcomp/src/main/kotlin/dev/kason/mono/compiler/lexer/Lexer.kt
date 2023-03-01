package dev.kason.mono.compiler.lexer

import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.error.CompileException
import mu.KLogging

// reads tokens one at a time
class Lexer(val source: CodeSource) {
	var index = 0

	fun readToken(): Token {
		val startIndex = index
		if (index >= source.content.length) return TerminalToken(source.index(startIndex))
		val firstCharacter = source.content[index]
		if (firstCharacter in LexerConstants.NUMBER_VALID_FIRST_LETTERS) {
			val token = testNumericLiteral(firstCharacter)
			if (token != null) return token
		}
		if (firstCharacter in LexerConstants.WORD_VALID_FIRST_LETTERS) {
			val word = readWhile { it in LexerConstants.IDENTIFIER_VALID_LETTERS }
			if (word in LexerConstants.HARD_KEYWORDS) return KeywordToken(word, source.index(startIndex))
			if (word in LexerConstants.MODIFIER_KEYWORDS) return ModifierToken(word, source.index(startIndex))
			return IdentifierToken(word, source.index(startIndex), hasBackticks = false)
		} else if (firstCharacter == '`') {
			return backtickIdentifier()
		}
		if (firstCharacter in LexerConstants.SINGLE_CHARACTER_PUNCTUATION) {
			val token = PunctuationToken(firstCharacter.toString(), source.index(startIndex))
			index++
			return token
		}
		if (index != source.content.length - 1 &&
			(firstCharacter.toString() + source.content[index + 1]) in LexerConstants.DOUBLE_CHARACTER_PUNCTUATION
		) {
			val token =
				PunctuationToken(firstCharacter.toString() + source.content[index + 1], source.index(startIndex))
			index += 2
			return token
		}
		if (firstCharacter == '\'') {
			return singleQuoteStringLiteral()
		} else if (firstCharacter == '"') {
			return doubleQuoteStringLiteral()
		}
		if (firstCharacter == '\n') return NewLineToken(source.index(index++))
		if (firstCharacter == '\t') {
			val number = readWhile { it == '\t' }.length
			return TabToken(number, source.index(startIndex))
		}
		if (firstCharacter == ' ') {
			val whitespace = readWhile { it == ' ' }
			return WhitespaceToken(whitespace, source.index(startIndex))
		}
		if (firstCharacter in LexerConstants.OPERATOR_CHARS) {
			return readOperator()
		}
		if (firstCharacter == '#') {
			return when {
				index != source.content.length - 1 && source.content[index + 1] == '(' -> readBlockComment()
				index != source.content.length - 1 && source.content[index + 1] == '[' -> readDocComment()
				else -> readLineComment()
			}
		}
		throw IllegalStateException("unknown character: $firstCharacter")
	}

	fun backtickIdentifier(): Token {
		val startIndex = index++
		val word = readWhile { it != '`' }
		if (index >= source.content.length) {
			TODO("error: unterminated backtick identifier")
		}
		index++
		return IdentifierToken(word, source.index(startIndex), hasBackticks = true)
	}

	fun readLineComment(): Token {
		val startIndex = index
		val comment = readWhile { it != '\n' }
		index++
		return CommentToken(comment, source.index(startIndex), CommentType.LINE)
	}

	fun readBlockComment(): Token {
		val startIndex = index
		val comment = readWhile { it != '#' || source.content[index - 1] != ')' }
		if (index >= source.content.length) {
			TODO("error: unterminated doc comment")
		}
		index++
		return CommentToken("$comment#", source.index(startIndex), CommentType.BLOCK)
	}

	fun readDocComment(): Token {
		val startIndex = index
		val comment = readWhile { it != '#' || source.content[index - 1] != ']' }
		if (index >= source.content.length) {
			TODO("error: unterminated doc comment")
		}
		index++
		return CommentToken("$comment#", source.index(startIndex), CommentType.DOC)
	}

	fun singleQuoteStringLiteral(): Token {
		val startIndex = index++
		val string = readWhile { it != '\n' && (it != '\'' || source.content[index - 1] != '\\') }
		if (index >= source.content.length || source.content[index] == '\n') {
			throw CompileException(UnterminatedDoubleQuoteStringLiteralContext(source.index(startIndex)))
		}
		index++
		return StringLiteralToken(string, source.index(startIndex), singleQuote = true)
	}

	fun readOperator(): Token {
		val startIndex = index
		val operator = readWhile { it in LexerConstants.OPERATOR_CHARS }
		return if (operator in LexerConstants.OPERATORS) {
			SimpleOperatorToken(operator, source.index(startIndex))
		} else {
			UnknownOperatorChainToken(operator, source.index(startIndex))
		}
	}

	fun doubleQuoteStringLiteral(): Token {
		val startIndex = index++
		val string = readWhile { it != '"' || source.content[index - 1] == '\\' }
		if (index >= source.content.length) {
			throw CompileException(UnterminatedDoubleQuoteStringLiteralContext(source.index(startIndex - 1)))
		}
		index++
		return StringLiteralToken(string, source.index(startIndex), singleQuote = false)
	}

	fun testNumericLiteral(firstChar: Char): Token? {
		val startIndex = index++
		val numberString = firstChar + readWhile { it in LexerConstants.NUMBER_VALID_LETTERS }
		val numberType =
			LexerConstants.NUMBER_LITERAL_TYPES.firstOrNull { it.regex matches numberString.replace("_", "") }
				?: return null
		return NumberLiteralToken(numberString, source.index(startIndex), numberType)
	}

	inline fun readWhile(predicate: (Char) -> Boolean): String {
		val builder = StringBuilder()
		while (index < source.content.length && predicate(source.content[index])) {
			builder.append(source.content[index++])
		}
		return builder.toString()
	}
}

object LexerConstants {

	val NUMBER_LITERAL_TYPES = NumberLiteralType.values()
	val NUMBER_VALID_FIRST_LETTERS = ('0'..'9') + '.'
	val NUMBER_VALID_LETTERS = ('0'..'9') + ('a'..'f') + ('A'..'F') + '.' + 'x' + 'X' + 'o' + 'O' + "_"

	val WORD_VALID_FIRST_LETTERS = ('a'..'z') + ('A'..'Z') + '_'
	val IDENTIFIER_VALID_LETTERS = WORD_VALID_FIRST_LETTERS + ('0'..'9')

	val SINGLE_CHARACTER_PUNCTUATION = listOf(
		'(', ')', '[', ']', '{', '}', ',', ';', ':', '.', "@"
	)

	val DOUBLE_CHARACTER_PUNCTUATION = listOf(
		"->", "::", "=>"
	)

	val HARD_KEYWORDS = listOf(
		"if",
		"else",
		"while",
		"for",
		"in",
		"do",
		"break",
		"continue",
		"return",
		"fn",
		"let",
		"var",
		"null",
		"true",
		"false",
		"class",
		"type",
		"not",
		"and",
		"or",
		"pass",
		"import",
		"try",
		"catch",
		"throw",
		"finally",
		"as",
		"is",
		"match"
	)

	val MODIFIER_KEYWORDS = listOf(
		"public",
		"private",
		"infix",
		"operator",
		"vararg",
		"external",
		"annotation"
	)

	val WHITESPACE = listOf(' ', '\r', '\u000C')
	val OPERATORS = listOf(
		"+", "-", "*", "/", "%",
		"==", "!=", "<", ">", "<=", ">=",
		"..", "++", "--", "===", "!=="
	)

	val OPERATOR_CHARS = OPERATORS.flatMap { it.toList() }.toSet()
}

fun Lexer.readAllTokens(): List<Token> {
	val tokens = mutableListOf<Token>()
	var token: Token
	do {
		token = readToken()
		tokens.add(token)
	} while (token !is TerminalToken)
	return tokens
}