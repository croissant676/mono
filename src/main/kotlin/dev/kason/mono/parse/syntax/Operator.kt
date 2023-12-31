package dev.kason.mono.parse.syntax

import dev.kason.mono.core.rangeTo
import dev.kason.mono.parse.lexing.Token

interface Operator {
	val symbol: String
	val precedence: Int
	val associativity: Associativity

	enum class Associativity {
		Left, Right
	}
}

enum class ArithmaticOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	Addition("+", 1000, Operator.Associativity.Left),
	Subtraction("-", 1000, Operator.Associativity.Left),
	Multiplication("*", 1100, Operator.Associativity.Left),
	Division("/", 1100, Operator.Associativity.Left),
	Exponentiation("**", 1200, Operator.Associativity.Right),
	Modulo("%", 1100, Operator.Associativity.Left)
}

enum class PrefixOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	Positive("+", 1300, Operator.Associativity.Right),
	Negative("-", 1300, Operator.Associativity.Right),
	Not("!", 1300, Operator.Associativity.Right),
	PreIncrement("++", 1350, Operator.Associativity.Right),
	PreDecrement("--", 1350, Operator.Associativity.Right),
	Spread("*", 250, Operator.Associativity.Right),
	Mut("mut", 1300, Operator.Associativity.Right),
	Var("var", 1300, Operator.Associativity.Right),
}

enum class PostfixOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity,
) : Operator {
	PostIncrement("++", 1400, Operator.Associativity.Right),
	PostDecrement("--", 1400, Operator.Associativity.Right),
}

enum class ComparisonOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	GreaterThan(">", 500, Operator.Associativity.Left),
	GreaterThanOrEqual(">=", 500, Operator.Associativity.Left),
	LessThan("<", 500, Operator.Associativity.Left),
	LessThanOrEqual("<=", 500, Operator.Associativity.Left),
	Equal("==", 400, Operator.Associativity.Left),
	NotEqual("!=", 400, Operator.Associativity.Left),
	Identity("===", 400, Operator.Associativity.Left),
	NegatedIdentity("!==", 400, Operator.Associativity.Left)
}

enum class LogicalOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	And("&", 350, Operator.Associativity.Left),
	Or("|", 300, Operator.Associativity.Left)
}

enum class NamedOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	Elvis("?:", 700, Operator.Associativity.Right),
	Range("..", 900, Operator.Associativity.Left)
}

enum class TypeOperator(
	override val symbol: String,
	override val precedence: Int,
	override val associativity: Operator.Associativity
) : Operator {
	Is("is", 600, Operator.Associativity.Left),
	NotIs("!is", 600, Operator.Associativity.Left),
	As("as", 1250, Operator.Associativity.Left),
	SafeAs("as?", 1250, Operator.Associativity.Left),
	Conversion(":", 1250, Operator.Associativity.Right)
}

class AssignmentOperator private constructor(val forOperator: Operator? = null) : Operator {
	override val symbol: String = (forOperator?.symbol ?: "") + "="
	override val precedence: Int = 100
	override val associativity: Operator.Associativity = Operator.Associativity.Right

	companion object {
		val operatorMap: Map<Operator?, AssignmentOperator> = buildMap {
			val possibleOperators =
				ArithmaticOperator.values().toList() + LogicalOperator.values() + null
			for (op in possibleOperators) {
				put(op, AssignmentOperator(op))
			}
		}

		operator fun get(operator: Operator?): AssignmentOperator? = operatorMap[operator]
		val Assignment = operatorMap[null]!!
	}

	override fun toString(): String = (forOperator?.symbol ?: "") + " Assign"
}

class BinaryOperatorNode(
	val operator: Operator,
	val left: Expression,
	val right: Expression
) : Expression(left, right) {
	override fun toString(): String = "operator '${operator.symbol}' applied on $left and $right"
}

class BinaryOperatorParselet(val operator: Operator) : InfixParselet<Expression> {
	override val precedence: Int get() = operator.precedence

	// subtract 1 from precedence if operator is right
	val effectivePrecedence: Int get() = precedence - operator.associativity.ordinal
	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val right = syntaxParser.parsePrecedence(effectivePrecedence)
		return BinaryOperatorNode(operator, left, right)
	}

	override fun toString(): String = "operator parselet for '${operator.symbol}'"
}

val binaryOperators = AssignmentOperator.operatorMap.values +
	ArithmaticOperator.values() +
	ComparisonOperator.values() +
	LogicalOperator.values() +
	NamedOperator.values()

class TypeBinaryOperatorNode(
	val operator: TypeOperator,
	val left: Expression,
	val right: TypeExpression
) : Expression(left, right) {
	override fun toString(): String = "operator '${operator.symbol}' applied on $left and $right"
}

class TypeBinaryOperatorParselet(val operator: TypeOperator) : InfixParselet<Expression> {
	override val precedence: Int get() = operator.precedence

	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression {
		val right = syntaxParser.parseTypeExpression()
		return TypeBinaryOperatorNode(operator, left, right)
	}

	override fun toString(): String = "operator parselet for '${operator.symbol}'"
}

fun DynamicParseletRegistry<InfixParselet<Expression>>.addBinaryOperatorParselets() {
	for (operator in binaryOperators) {
		this on { it.text == operator.symbol } use BinaryOperatorParselet(operator)
	}
	for (typeOperator in TypeOperator.values()) {
		this on { it.text == typeOperator.symbol } use TypeBinaryOperatorParselet(typeOperator)
	}
}

class PrefixExpression(
	val operator: PrefixOperator,
	val operatorToken: Token,
	val right: Expression
) : Expression(operatorToken.range..right.range) {
	init {
		this += right
	}

	override fun toString(): String = "prefix operator '${operator.symbol}' applied on $right"
}

class PrefixOperatorParselet(val operator: PrefixOperator) : PrefixParselet<Expression> {
	override fun parse(syntaxParser: SyntaxParser, token: Token): Expression {
		val right = syntaxParser.parsePrecedence(operator.precedence)
		return PrefixExpression(operator, token, right)
	}

	override fun toString(): String = "prefix operator parselet for '${operator.symbol}'"
}

fun DynamicParseletRegistry<PrefixParselet<Expression>>.addPrefixOperatorParselets() {
	for (prefix in PrefixOperator.values()) {
		this.on { it.text == prefix.symbol } use PrefixOperatorParselet(prefix)
	}
}

class PostfixExpression(
	val operator: PostfixOperator,
	val operatorToken: Token,
	val left: Expression
) : Expression(left.range..operatorToken.range) {
	init {
		this += left
	}

	override fun toString(): String = "postfix operator '${operator.symbol}' applied on $left"
}

class PostfixOperatorParselet(val operator: PostfixOperator) : InfixParselet<Expression> {
	override val precedence: Int get() = operator.precedence

	override fun parse(syntaxParser: SyntaxParser, left: Expression, token: Token): Expression =
		PostfixExpression(operator, token, left)

	override fun toString(): String = "postfix operator parselet for '${operator.symbol}'"
}

fun DynamicParseletRegistry<InfixParselet<Expression>>.addPostfixOperatorParselets() {
	for (postfix in PostfixOperator.values()) {
		this on { it.text == postfix.symbol } use PostfixOperatorParselet(postfix)
	}
}