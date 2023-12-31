package dev.kason.mono.parse.lexing

import dev.kason.mono.core.Document
import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.read

class Token(val tokenKind: TokenKind, val range: IndexRange) {
	val text: String = range.read()
	val document: Document get() = range.document
	override fun toString(): String =
		"'$tokenKind' token ($range, '${text.escape()}')"
}

sealed interface TokenKind

enum class TokenKinds : TokenKind {
	EOF,
	Identifier,
	Keyword,
	Indent,
	Dedent, // created in post
	Newline,
	Whitespace,
	Symbol,
	Unknown;
}

enum class Base {
	Bin,
	Oct,
	Dec,
	Hex;

	companion object {
		operator fun get(char: Char): Base? = when (char) {
			'b' -> Bin
			'o' -> Oct
			'd' -> Dec
			'x' -> Hex
			else -> null
		}
	}
}

enum class CommentTokenKind : TokenKind {
	Line,
	Block,
	DocLine,
	DocBlock;
}

sealed interface NumberLiteralTokenKind : TokenKind

data class IntLiteralTokenKind(
	val base: Base,
	val isEmpty: Boolean // if the parsed text has no numerical value (like 0b)
) : NumberLiteralTokenKind {
	override fun toString(): String =
		if (isEmpty) "$base int (empty)" else "$base int"
}

data class FloatLiteralTokenKind(
	val base: Base,
	val hasExponent: Boolean
) : NumberLiteralTokenKind {
	override fun toString(): String = "$base float ${if (hasExponent) "with" else ", no"} exponent"
}

enum class LiteralTokenKinds : TokenKind {
	Char,
	Boolean;

	override fun toString(): String = name.lowercase()
}

data class StringLiteralTokenKind(
	val startCurly: Boolean,
	val endCurly: Boolean,
	val identCount: Int = 0
): TokenKind