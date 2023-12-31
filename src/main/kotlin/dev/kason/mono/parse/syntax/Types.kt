package dev.kason.mono.parse.syntax

import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.rangeTo
import dev.kason.mono.parse.lexing.*

open class TypeExpression(range: IndexRange) : SyntaxNode(range) {
	constructor(vararg nodes: SyntaxNode) : this(IndexRange(nodes.map { it.range })) {
		for (node in nodes) {
			plusAssign(node)
		}
	}
}

class IdentifierTypeExpression(val token: Token) : TypeExpression(token.range) {
	override fun toString(): String = "IdentifierTypeExpression($text)"
}

object IdentifierTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression = IdentifierTypeExpression(token)
}

class GenericsTypeExpression(
	val left: TypeExpression,
	val leftAngle: Token,
	val generics: List<TypeExpression>,
	val rightAngle: Token
) : TypeExpression(left.range..rightAngle.range) {
	init {
		this += left
		this += generics
	}

	override fun toString(): String = "GenericsTypeExpression($left, $generics)"
}

object GenericsTypeParselet : InfixParselet<TypeExpression> {
	override val precedence: Int get() = 1500

	override fun parse(syntaxParser: SyntaxParser, left: TypeExpression, token: Token): TypeExpression {
		val generics = syntaxParser.parseGenerics()
		val rightAngle = syntaxParser.cursor.expect { it.text == ">" }
		return GenericsTypeExpression(left, token, generics, rightAngle)
	}
}

class TagTypesExpression(
	val left: TypeExpression,
	val leftParen: Token,
	val tags: List<Expression>,
	val rightParen: Token
) : TypeExpression(left.range..rightParen.range) {

	init {
		this += left
		this += tags
	}
	override fun toString(): String = "TagTypesExpression($left, $tags)"
}

// parse something like left(tag1, tag2, tag3)
// note that tags can't be tuples or assignments
// but we can do something like String(length = 5)
object TagTypesParselet : InfixParselet<TypeExpression> {
	override val precedence: Int get() = 1500

	override fun parse(syntaxParser: SyntaxParser, left: TypeExpression, token: Token): TypeExpression {
		val tags = syntaxParser.parsePossibleNamedArgumentList("(")
		val rightCurly = syntaxParser.cursor.expect { it.text == ")" }
		return TagTypesExpression(left, token, tags, rightCurly)
	}
}

class ParenthesesTypeExpression(val inner: TypeExpression) : TypeExpression(inner) {
	override fun toString(): String = "ParenthesesTypeExpression($inner)"
}

object ParenthesesTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression {
		val inner = syntaxParser.parseTypeExpression()
		syntaxParser.cursor.expect { it.text == ")" }
		return ParenthesesTypeExpression(inner)
	}
}

class TupleTypeExpression(
	val leftParen: Token,
	val typeExpressions: List<TypeExpression>,
	val rightParen: Token
) : TypeExpression(leftParen.range..rightParen.range) {
	init {
		for (typeExpression in typeExpressions) {
			plusAssign(typeExpression)
		}
	}

	override fun toString(): String = "TupleTypeExpression($typeExpressions)"
}

object TupleTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression {
		val typeExpressions = mutableListOf<TypeExpression>()
		while (syntaxParser.cursor.hasNext()) {
			typeExpressions += syntaxParser.parseTypeExpression()
			if (syntaxParser.cursor.current?.text == ")") break
			syntaxParser.cursor.expect { it.text == "," }
		}
		val rightParen = syntaxParser.cursor.expect { it.text == ")" }
		return TupleTypeExpression(token, typeExpressions, rightParen)
	}
}

class AndTypeExpression(
	val left: TypeExpression,
	val andToken: Token,
	val right: TypeExpression
) : TypeExpression(left, right) {
	override fun toString(): String = "AndTypeExpression($left, $right)"
}

object AndTypeParselet : InfixParselet<TypeExpression> {
	override val precedence: Int get() = 1000

	override fun parse(syntaxParser: SyntaxParser, left: TypeExpression, token: Token): TypeExpression {
		val right = syntaxParser.parseTypeExpression()
		return AndTypeExpression(left, token, right)
	}
}

class OrTypeExpression(
	val left: TypeExpression,
	val orToken: Token,
	val right: TypeExpression
) : TypeExpression(left, right) {
	override fun toString(): String = "OrTypeExpression($left, $right)"
}

object OrTypeParselet : InfixParselet<TypeExpression> {
	override val precedence: Int get() = 800

	override fun parse(syntaxParser: SyntaxParser, left: TypeExpression, token: Token): TypeExpression {
		val right = syntaxParser.parseTypeExpression()
		return OrTypeExpression(left, token, right)
	}
}

class StringTypeExpression(val token: Token) : TypeExpression(token.range) {
	override fun toString(): String = "StringTypeExpression($text)"
}

object StringTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression =
		StringTypeExpression(token)
}

class IntConstTypeExpression(val token: Token) : TypeExpression(token.range) {
	override fun toString(): String = "IntConstTypeExpression($text)"
}

object IntConstTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression =
		IntConstTypeExpression(token)
}

class MutTypeExpression(val mutToken: Token, val inner: TypeExpression) : TypeExpression(mutToken.range..inner.range) {
	override fun toString(): String = "MutTypeExpression($inner)"
}

object MutTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression {
		val inner = syntaxParser.parseTypeExpression()
		return MutTypeExpression(token, inner)
	}
}

class VarTypeExpression(val varToken: Token, val inner: TypeExpression) : TypeExpression(varToken.range..inner.range) {
	override fun toString(): String = "VarTypeExpression($inner)"
}

object VarTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression {
		val inner = syntaxParser.parseTypeExpression()
		return VarTypeExpression(token, inner)
	}
}

class WildcardTypeExpression(val token: Token) : TypeExpression(token.range) {
	override fun toString(): String = "WildcardExpression($text)"
}

object WildcardTypeParselet : PrefixParselet<TypeExpression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): TypeExpression =
		WildcardTypeExpression(token)
}

fun DynamicParseletRegistry<PrefixParselet<TypeExpression>>.registerTypePrefixParselets() = apply {
	on { it.text == "mut" } use MutTypeParselet
	on { it.text == "var" } use VarTypeParselet
	on { it.tokenKind == TokenKinds.Identifier } use IdentifierTypeParselet
	on { it.text == "(" } use ParenthesesTypeParselet
	on(10) { it.text == "(" } use TupleTypeParselet
	on { it.tokenKind is StringLiteralTokenKind } use StringTypeParselet
	on { it.tokenKind is IntLiteralTokenKind } use IntConstTypeParselet
	on { it.text == "*" } use WildcardTypeParselet
}

fun DynamicParseletRegistry<InfixParselet<TypeExpression>>.registerTypeInfixParselets() = apply {
	on { it.text == "<" } use GenericsTypeParselet
	on { it.text == "{" } use TagTypesParselet
	on { it.text == "&" } use AndTypeParselet
	on { it.text == "|" } use OrTypeParselet
}