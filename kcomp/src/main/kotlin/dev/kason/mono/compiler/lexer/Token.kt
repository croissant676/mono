package dev.kason.mono.compiler.lexer

import dev.kason.mono.compiler.base.CodeIndex
import dev.kason.mono.compiler.base.CodeRange

sealed class Token(val value: String, val index: CodeIndex) {
	val range: CodeRange get() = index..(index + value.length)

	override fun hashCode(): Int = value.hashCode() + index.hashCode()
	override fun equals(other: Any?): Boolean {
		if (other !is Token) return false
		return value == other.value && index == other.index
	}

	override fun toString(): String = "Token(type=$tokenType, value=$value, index=$index)"
}

class IdentifierToken(
	value: String, index: CodeIndex,
	val hasBackticks: Boolean
) : Token(value, index) {
	val name: String = if (hasBackticks) value.substring(1, value.length - 1) else value
	override fun toString(): String =
		"IdentifierToken(type=$tokenType, value=$value, index=$index, hasBackticks=$hasBackticks)"
}

class KeywordToken(value: String, index: CodeIndex) : Token(value, index)
class ModifierToken(value: String, index: CodeIndex) : Token(value, index)

// some keywords are just modifiers for other keywords
// modifiers can be used as identifiers in certain contexts
class SimpleOperatorToken(value: String, index: CodeIndex) : Token(value, index)

class UnknownOperatorChainToken(
	value: String, index: CodeIndex
) : Token(value, index) // delegate to the parser to figure out what this is

class StringLiteralToken(
	value: String, index: CodeIndex,
	val singleQuote: Boolean
) : Token(value, index) {
	override fun toString(): String =
		"StringLiteralToken(type=$tokenType, value=$value, index=$index, singleQuote=$singleQuote)"
}

class NumberLiteralToken(
	value: String, index: CodeIndex,
	val type: NumberLiteralType
) : Token(value, index) {
	override fun toString(): String = "NumberLiteralToken(type=$tokenType, value=$value, index=$index, type=$type)"
}

enum class NumberLiteralType(val regex: Regex) {
	BINARY(Regex("0[bB][01]+")),
	OCTAL(Regex("0[oO]?[0-7]+")),
	HEXADECIMAL(Regex("0[xX][0-9a-fA-F]+")),
	FLOATING_POINT(Regex("(0|[1-9][0-9]*)(\\.[0-9]+)?")),
	EXPONENT(Regex("(0|[1-9][0-9]*)(\\.[0-9]+)?[eE][+-]?\\d+")),
	DECIMAL(Regex("(0|[1-9][0-9]*)"));

	val base: Int
		get() = when (this) {
			BINARY -> 2
			OCTAL -> 8
			HEXADECIMAL -> 16
			else -> 10
		}
}

class PunctuationToken(value: String, index: CodeIndex) : Token(value, index)
class CommentToken(
	value: String, index: CodeIndex,
	val type: CommentType
) : Token(value, index) {
	override fun toString(): String = "CommentToken(type=$tokenType, value=$value, index=$index, type=$type)"
}

enum class CommentType {
	LINE,
	BLOCK,
	DOC
}

class TabToken(val number: Int, index: CodeIndex) : Token("\t".repeat(number), index) {
	override fun toString(): String = "TabToken(type=$tokenType, index=$index, number=$number)"
}

class NewLineToken(index: CodeIndex) : Token("\n", index) {
	override fun toString(): String = "NewLineToken(type=$tokenType, index=$index)"
}

class WhitespaceToken(value: String, index: CodeIndex) : Token(value, index)
class TerminalToken(index: CodeIndex) : Token("<end of source>", index) {
	override fun toString(): String = "TerminalToken(index=${index.index})"
}

enum class TokenType {
	IDENTIFIER,
	KEYWORD,
	MODIFIER,
	SIMPLE_OPERATOR,
	UNKNOWN_OPERATOR_CHAIN,
	STRING_LITERAL,
	NUMBER_LITERAL,
	PUNCTUATION,
	COMMENT,
	TAB,
	NEW_LINE,
	WHITESPACE,
	TERMINAL;

	override fun toString(): String = name.lowercase().replace('_', ' ')
}

val Token.tokenType: TokenType
	get() = when (this) {
		is IdentifierToken -> TokenType.IDENTIFIER
		is KeywordToken -> TokenType.KEYWORD
		is ModifierToken -> TokenType.MODIFIER
		is SimpleOperatorToken -> TokenType.SIMPLE_OPERATOR
		is UnknownOperatorChainToken -> TokenType.UNKNOWN_OPERATOR_CHAIN
		is StringLiteralToken -> TokenType.STRING_LITERAL
		is NumberLiteralToken -> TokenType.NUMBER_LITERAL
		is PunctuationToken -> TokenType.PUNCTUATION
		is CommentToken -> TokenType.COMMENT
		is TabToken -> TokenType.TAB
		is NewLineToken -> TokenType.NEW_LINE
		is WhitespaceToken -> TokenType.WHITESPACE
		is TerminalToken -> TokenType.TERMINAL
	}

