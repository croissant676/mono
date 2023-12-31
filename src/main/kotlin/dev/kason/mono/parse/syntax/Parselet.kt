package dev.kason.mono.parse.syntax

import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.contract
import dev.kason.mono.core.rangeTo
import dev.kason.mono.core.read
import dev.kason.mono.parse.lexing.StringLiteralTokenKind
import dev.kason.mono.parse.lexing.Token
import dev.kason.mono.parse.lexing.TokenKinds

sealed interface Parselet<T> where T: SyntaxNode

interface PrefixParselet<T : SyntaxNode>: Parselet<T> {
	fun parse(syntaxParser: SyntaxParser, token: Token): T
}

interface InfixParselet<T : SyntaxNode>: Parselet<T> {
	fun parse(
		syntaxParser: SyntaxParser, left: T, token: Token
	): T

	val precedence: Int
}

class IntLiteralNode(val token: Token) : Expression(token.range) {
	override fun toString(): String = "int literal '${token.text}'"
}

object IntLiteralParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression = IntLiteralNode(token)
}

class FloatLiteralNode(val token: Token) : Expression(token.range) {
	override fun toString(): String = "float literal '${token.text}'"
}

object FloatLiteralParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression = FloatLiteralNode(token)
}

class StringLiteralNode(range: IndexRange) : Expression(range) {
	constructor(token: Token) : this(token.range.contract(1))

	override fun toString(): String = "string literal '${range.read()}'"
}

class StringBuilderLiteralNode(val expressions: List<Expression>) : Expression(* expressions.toTypedArray()) {

	override fun toString(): String = "string builder literal '${expressions.joinToString(" + ") { it.text }}'"
}

object StringLiteralParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression {
		var currentToken = token
		var tokenKind = currentToken.tokenKind as StringLiteralTokenKind
		// startCurly must be false here
		if (!tokenKind.endCurly && tokenKind.identCount == 0) return StringLiteralNode(currentToken)
		val expressions = mutableListOf<Expression>()
		val identTokens = syntaxParser.cursor.next(tokenKind.identCount)
		check(identTokens.all { it.tokenKind == TokenKinds.Identifier }) {
			"expected all tokens to be identifiers"
		}
		expressions.addAll(splitStringLiteral(currentToken, identTokens))
		while (tokenKind.endCurly) {
			expressions.add(syntaxParser.parseExpression())
			currentToken = syntaxParser.cursor.expect { it.tokenKind is StringLiteralTokenKind }
			tokenKind = currentToken.tokenKind as StringLiteralTokenKind
			expressions.addAll(splitStringLiteral(currentToken, syntaxParser.cursor.next(tokenKind.identCount)))
		}
		return StringBuilderLiteralNode(expressions)
	}

	// right now, ident interpolation is represented as
	// a string literal token followed by identifier tokens
	// we want to split the string literals into parts by the identifier token locations
	// so that we can get an easy list to add to our string builder literal node
	// input:
	// "hi $name lets add another variable $another end"
	// output:
	// ["hi ", name, " lets add another variable ", another, " end"]
	fun splitStringLiteral(literalToken: Token, identTokens: List<Token>): List<Expression> {
		var startIndex = literalToken.range.start + 1 // start of text after "
		val parts = mutableListOf<Expression>()
		for (identToken in identTokens) {
			val endIndex = identToken.range.start - 2 // get rid of $ prefix and account for inclusive range
			if (startIndex != endIndex) {
				parts += StringLiteralNode(startIndex..endIndex)
			}
			parts += IdentifierNode(identToken)
			startIndex = identToken.range.endInclusive + 1 // start of text after
		}
		if (startIndex != literalToken.range.endInclusive) {
			val distOff = if ((literalToken.tokenKind as StringLiteralTokenKind).endCurly) 2 else 1
			parts += StringLiteralNode(startIndex..literalToken.range.endInclusive - distOff)
		}
		parts.removeAll { it.range.isEmpty() }
		return parts
	}
}

class BooleanLiteralNode(val token: Token) : Expression(token.range) {
	override fun toString(): String = "boolean literal '${token.text}'"
}

object BooleanLiteralParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression = BooleanLiteralNode(token)
}

class CharLiteralNode(val token: Token) : Expression(token.range) {
	override fun toString(): String = "char literal '${token.text}'"
}

object CharLiteralParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression = CharLiteralNode(token)
}

class IdentifierNode(val token: Token) : Expression(token.range) {
	override fun toString(): String = "identifier '${token.text}'"
}

object IdentifierParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression = IdentifierNode(token)
}

class ParenNode(
	val left: Token,
	val expression: Expression,
	val right: Token
) : Expression(left.range..right.range) {

	init {
		this += expression
	}

	override fun toString(): String = "parentheses around '$expression'"
}

object ParenParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression {
		val expression = syntaxParser.parseExpression()
		val right = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Symbol && it.text == ")" }
		return ParenNode(token, expression, right)
	}
}

class MemberAccessNode(
	val left: Expression,
	val dot: Token,
	val right: IdentifierNode
) : Expression(left, right) {
	override fun toString(): String = "member access '$left.$right'"
}

object MemberAccessParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 2000
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val right = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Identifier }
		return MemberAccessNode(left, token, IdentifierNode(right))
	}
}

class TupleNode(
	val left: Expression,
	val comma: Token,
	val right: Expression
) : Expression(left, right) {

	override fun toString(): String = "$left, $right"
}

object TupleParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 200
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val right = syntaxParser.parseExpression()
		return TupleNode(left, token, right)
	}
}

class CallNode(
	val left: Expression,
	val leftParen: Token,
	val arguments: List<Expression>,
	val rightParen: Token
) : Expression(left.range..rightParen.range) {
	init {
		this += left
		this += arguments
	}

	override fun toString(): String = "call '$left' with arguments $arguments"
}

object CallParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 2000
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		if (syntaxParser.cursor.current?.text == ")") {
			val rightParen = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Symbol && it.text == ")" }
			return CallNode(left, token, emptyList(), rightParen)
		}
		val arguments = syntaxParser.parsePossibleNamedArgumentList(")")
		val rightParen = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Symbol && it.text == ")" }
		return CallNode(left, token, arguments, rightParen)
	}
}

class GenericReferenceNode(
	val left: Expression,
	val leftAngle: Token,
	val arguments: List<TypeExpression>,
	val rightAngle: Token
) : Expression(left.range..rightAngle.range) {
	init {
		this += left
		this += arguments
	}

	override fun toString(): String = "generic reference '$left<$arguments>'"
}

object GenericReferenceParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 2000
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val arguments = syntaxParser.parseGenerics()
		val rightAngle = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Symbol && it.text == ">" }
		return GenericReferenceNode(left, token, arguments, rightAngle)
	}
}

class ArrayAccessNode(
	val left: Expression,
	val leftBracket: Token,
	val index: Expression,
	val rightBracket: Token
) : Expression(left.range..rightBracket.range) {
	init {
		this += left
		this += index
	}

	override fun toString(): String = "array access '$left[$index]'"
}

object ArrayAccessParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 2000
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val index = syntaxParser.parseExpression()
		val rightBracket = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Symbol && it.text == "]" }
		return ArrayAccessNode(left, token, index, rightBracket)
	}
}

class TernaryNode(
	val condition: Expression,
	val ifToken: Token,
	val trueExpression: Expression,
	val elseToken: Token,
	val falseExpression: Expression
) : Expression(condition, trueExpression, falseExpression) {
	override fun toString(): String = "ternary '$trueExpression if $condition else $falseExpression'"
}

object TernaryParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 150
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val condition = syntaxParser.parseExpression()
		val elseToken = syntaxParser.cursor.expect { it.text == "else" }
		val falseExpression = syntaxParser.parseExpression()
		return TernaryNode(condition, token, left, elseToken, falseExpression)
	}
}

class InfixCallNode(
	val left: Expression, val infixCall: IdentifierNode, val right: Expression
) : Expression(left, infixCall, right) {
	override fun toString(): String = "infix call '$left $infixCall $right'"
}

object InfixCallParselet : InfixParselet<Expression> {
	override val precedence: Int get() = 750
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val right = syntaxParser.parseExpression()
		return InfixCallNode(left, IdentifierNode(token), right)
	}
}

object ConditionalParselet : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression =
		syntaxParser.parseConditionalGroup()
}