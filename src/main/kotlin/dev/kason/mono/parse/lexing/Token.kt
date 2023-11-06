package dev.kason.mono.parse.lexing

import dev.kason.mono.core.Document
import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.read

class Token(val tokenKind: TokenKind, val range: IndexRange) {
	val text: String get() = range.read()
	val document: Document get() = range.document
	override fun toString(): String =
		"'$tokenKind' token ($range, '${text.escape()}')"
}

sealed interface TokenKind

enum class TokenKinds : TokenKind {
	EOF,
	Identifier, // includes keywords
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
	String,
	Char,
	Boolean;

	override fun toString(): kotlin.String = name.lowercase()
}

// token predicate

typealias TokenPredicate = (Token) -> Boolean

inline infix fun Token.satisfies(tokenPredicate: TokenPredicate): Boolean = tokenPredicate(this)
inline infix fun TokenCursor.satisfies(tokenPredicate: TokenPredicate): Boolean =
	peek()?.satisfies(tokenPredicate) ?: false

class HasTextTokenPredicate internal constructor(val text: String) : TokenPredicate {
	override fun invoke(token: Token): Boolean {
		if (token.tokenKind != TokenKinds.Symbol) return token.text == text
		return token.text.startsWith(text)
	}
}

private val hasTextPredicateCache: MutableMap<String, HasTextTokenPredicate> = mutableMapOf()

fun hasText(text: String): HasTextTokenPredicate =
	hasTextPredicateCache.getOrPut(text) { HasTextTokenPredicate(text) }

fun hasType(type: TokenKind): TokenPredicate = { it.tokenKind == type }
inline fun hasType(crossinline typePredicate: (TokenKind) -> Boolean): TokenPredicate = { typePredicate(it.tokenKind) }
inline fun <reified T> hasType(): TokenPredicate = { it.tokenKind is T }

inline infix fun TokenPredicate.and(crossinline other: TokenPredicate): TokenPredicate = { this(it) && other(it) }
inline infix fun TokenPredicate.or(crossinline other: TokenPredicate): TokenPredicate = { this(it) || other(it) }
fun TokenPredicate.not(): TokenPredicate = { !this(it) }

// it's probably better to not use these, but they might be needed for parsing