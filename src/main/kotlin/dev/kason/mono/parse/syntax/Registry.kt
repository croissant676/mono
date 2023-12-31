package dev.kason.mono.parse.syntax

import dev.kason.mono.parse.lexing.*
import java.util.*

interface ParseletRegistry<out T> where T : Parselet<*> {
	/** Gets the parselet for the token at the given position in the token stream. */
	operator fun get(tokenStream: TokenStream, position: Int): T? =
		this[tokenStream[position]]

	operator fun get(token: Token): T?
	operator fun get(maxPrecedence: Int, token: Token): T? = this[token]
}

/**
 * Default parselet registry - each parselet is associated with a condition and
 * precedence. Returns the highest precedence parselet that matches the condition.
 * */
class DynamicParseletRegistry<out T : Parselet<*>> : ParseletRegistry<T> {
	internal data class ParseletOrdering<T>(
		val relativeValue: Int,
		val condition: (Token) -> Boolean,
		val parselet: T
	)

	private val orderings: MutableSet<ParseletOrdering<T>> =
		TreeSet(compareByDescending<ParseletOrdering<T>> { it.relativeValue }
			.thenBy { it.parselet.toString() })

	override fun get(token: Token): T? {
		for (ordering in orderings) {
			if (ordering.condition(token)) {
				return ordering.parselet
			}
		}
		return null
	}

	override fun get(maxPrecedence: Int, token: Token): T? {
		for (ordering in orderings) {
			if (ordering.relativeValue < maxPrecedence
				&& ordering.condition(token)
			) {
				return ordering.parselet
			}
		}
		return null
	}


	interface OrderingBuilder<out T> {
		infix fun use(parselet: @UnsafeVariance T): Boolean
	}

	// dsl for adding parselets
	fun on(relativeValue: Int = 0, condition: (Token) -> Boolean) = object : OrderingBuilder<T> {
		override fun use(parselet: @UnsafeVariance T) =
			orderings.add(ParseletOrdering(relativeValue, condition, parselet))
	}
}

// same thing as before, but for infix it uses the precedence of the parselet
inline infix fun <reified T : SyntaxNode>
	DynamicParseletRegistry<InfixParselet<T>>.on(noinline condition: (Token) -> Boolean) =
	object : DynamicParseletRegistry.OrderingBuilder<InfixParselet<T>> {
		override fun use(parselet: InfixParselet<T>): Boolean {
			val relativeValue = parselet.precedence
			return this@on.on(relativeValue, condition) use parselet
		}
	}


fun DynamicParseletRegistry<PrefixParselet<Expression>>.registerPrefixParselets() = apply {
	on { it.tokenKind is IntLiteralTokenKind } use IntLiteralParselet
	on { it.tokenKind is FloatLiteralTokenKind } use FloatLiteralParselet
	on { it.tokenKind is StringLiteralTokenKind } use StringLiteralParselet
	on { it.tokenKind == LiteralTokenKinds.Boolean } use BooleanLiteralParselet
	on { it.tokenKind == LiteralTokenKinds.Char } use CharLiteralParselet
	on { it.tokenKind == TokenKinds.Identifier } use IdentifierParselet
	on { it.text == "(" } use ParenParselet
	on { it.text == "if" } use ConditionalParselet
	addPrefixOperatorParselets()
}

fun DynamicParseletRegistry<InfixParselet<Expression>>.registerInfixParselets() = apply {
	addBinaryOperatorParselets()
	addPostfixOperatorParselets()
	this on { it.text == "(" } use CallParselet
	this on { it.text == "<" } use GenericReferenceParselet
	this on { it.text == "[" } use ArrayAccessParselet
	this on { it.text == "." } use MemberAccessParselet
	this on { it.text == "," } use TupleParselet
	this on { it.text == "if" } use TernaryParselet
	this on { it.tokenKind == TokenKinds.Identifier } use InfixCallParselet
}

inline fun <reified T : Parselet<*>> DynamicParseletRegistry(initializer: DynamicParseletRegistry<T>.() -> Unit): DynamicParseletRegistry<T> {
	val registry = DynamicParseletRegistry<T>()
	registry.initializer()
	return registry
}
