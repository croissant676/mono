package dev.kason.mono.parse.syntax

import dev.kason.mono.parse.lexing.Token
import dev.kason.mono.parse.lexing.TokenCursor
import dev.kason.mono.parse.lexing.TokenKinds
import dev.kason.mono.parse.lexing.TokenStream

// algorithm: https://tinyurl.com/2ndwrhkn
class SyntaxParser(
	val prefixRegistry: ParseletRegistry<PrefixParselet<Expression>>,
	val infixRegistry: ParseletRegistry<InfixParselet<Expression>>,
	val typePrefixRegistry: ParseletRegistry<PrefixParselet<TypeExpression>>,
	val typeInfixRegistry: ParseletRegistry<InfixParselet<TypeExpression>>,
	tokenStream: TokenStream
) {
	val cursor: TokenCursor = TokenCursor(tokenStream)
	val currentToken: Token get() = cursor.current ?: error("no current token")

	fun precedence(registry: ParseletRegistry<InfixParselet<*>>) =
		if (cursor.current == null) 0 else registry[currentToken]?.precedence ?: 0

	fun parseExpression(): Expression = parsePrecedence(0)
	fun parsePrecedence(precedence: Int = 0): Expression {
		val token = cursor.eat() ?: error("no current token")
		val prefixParselet = prefixRegistry[token] ?: error("can't parse expression that starting with token '$token'")
		return parseInfix(prefixParselet.parse(this, token), infixRegistry, precedence)
	}

	fun parseTypeExpression(precedence: Int = 0): TypeExpression {
		val token = cursor.eat() ?: error("no current token")
		val prefixParselet = typePrefixRegistry[token] ?: error("can't parse type that starting with token '$token'")
		return parseInfix(prefixParselet.parse(this, token), typeInfixRegistry, precedence)
	}

	tailrec fun <T : SyntaxNode> parseInfix(left: T, registry: ParseletRegistry<InfixParselet<T>>, precedence: Int): T {
		if (precedence >= precedence(registry)) return left
		val token = cursor.current ?: error("no current token")
		var expression: T?
		var maxPrecedence = Int.MAX_VALUE
		do {
			val infixParselet = registry[maxPrecedence, token]
				?: return left
			expression = cursor.transaction {
				cursor.eat()
				return@transaction infixParselet.parse(this@SyntaxParser, left, token)
			}
			maxPrecedence = infixParselet.precedence
		} while (expression == null)
		return parseInfix(expression, registry, precedence)
	}

	internal fun parseWhileLoop(): WhileLoopNode {
		val whileToken = cursor.expect { it.text == "while" }
		val condition = parseExpression()
		val body = parseBlock()
		return WhileLoopNode(whileToken, condition, body)
	}

	internal fun parseDoWhileLoop(): DoWhileLoopNode {
		val doToken = cursor.expect { it.text == "do" }
		val body = parseBlock()
		cursor.eatNL()
		val whileToken = cursor.expect { it.text == "while" }
		val condition = parseExpression()
		return DoWhileLoopNode(doToken, body, whileToken, condition)
	}

	internal fun parseIfStatement(): ConditionalNode {
		cursor.eatNL()
		val ifToken = cursor.expect { it.text == "if" }
		cursor.eatNL()
		val condition = parseExpression()
		cursor.eatNL()
		val body = parseBlock()
		return ConditionalNode(ifToken, condition, body)
	}

	internal fun parseElseIfStatement(): ConditionalNode {
		val elseIfToken = cursor.expect { it.text == "elif" }
		cursor.eatNL()
		val condition = parseExpression()
		cursor.eatNL()
		val body = parseBlock()
		return ConditionalNode(elseIfToken, condition, body)
	}

	internal fun parseConditionalGroup(): ConditionalGroupNode {
		val conditionals = mutableListOf<ConditionalNode>()
		val ifStatementNode = parseIfStatement()
		conditionals += ifStatementNode
		cursor.eatNL()
		while (cursor.current?.text == "elif") {
			val elseIfStatementNode = parseElseIfStatement()
			conditionals += elseIfStatementNode
			cursor.eatNL()
		}
		if (cursor.current?.text == "else") {
			val elseToken = cursor.expect { it.text == "else" }
			val body = parseBlock()
			conditionals += ConditionalNode(elseToken, null, body)
		}
		return ConditionalGroupNode(conditionals)
	}

	internal fun parseForLoop(): ForLoopNode {
		cursor.eatNL()
		val forToken = cursor.expect { it.text == "for" }
		val identifiers = mutableListOf<IdentifierNode>()
		do {
			cursor.eatNL()
			identifiers += IdentifierNode(cursor.expect { it.tokenKind == TokenKinds.Identifier })
		} while (cursor.eatIf { it.text == "," } != null)
		cursor.eatNL()
		val inToken = cursor.expect { it.text == "in" }
		cursor.eatNL()
		val expression = parseExpression()
		cursor.eatNL()
		val body = parseBlock()
		return ForLoopNode(forToken, identifiers, inToken, expression, body)
	}

	fun parseBlock(): BlockNode {
		cursor.eatNL()
		val next = cursor.current
		if (next?.text == ":") {
			cursor.eat()
			return parseIndentedBlock()
		}
		val statement = parseExpression()
		return BlockNode(listOf(statement))
	}

	fun parseIndentedBlock(): BlockNode {
		cursor.eatNL()
		cursor.expect { it.tokenKind == TokenKinds.Indent }
		cursor.eatNL()
		val statements = mutableListOf<Statement>()
		while (cursor.hasNext() && cursor.current?.tokenKind != TokenKinds.Dedent) {
			statements += parseStatement()
			cursor.eatNL()
		}
		if (cursor.hasNext()) cursor.expect { it.tokenKind == TokenKinds.Dedent }
		return BlockNode(statements)
	}

	fun parseReturnStatement(): ReturnNode {
		val returnToken = cursor.expect { it.text == "return" }
		val label = parseLabelIfPossible()
		val expression  = parseExpression()
		return ReturnNode(returnToken, label, expression)
	}

	fun parseLabel(): Label {
		val leftSlash = cursor.expect { it.text == "/" }
		val identifier = cursor.expect { it.tokenKind == TokenKinds.Identifier }
		val rightSlash = cursor.expect { it.text == "/" }
		return Label(leftSlash, IdentifierNode(identifier), rightSlash)
	}

	fun parseLabelIfPossible(): Label? = cursor.transaction{
		parseLabel()
	}

	fun parseBreakStatement(): BreakNode {
		val breakToken = cursor.expect { it.text == "break" }
		val label = parseLabelIfPossible()
		return BreakNode(breakToken, label)
	}

	fun parseContinueStatement(): ContinueNode {
		val continueToken = cursor.expect { it.text == "continue" }
		val label = parseLabelIfPossible()
		return ContinueNode(continueToken, label)
	}

	fun tryParseConditionalStatement(statement: Statement): Statement {
		val ifToken = cursor.current ?: return statement
		if (ifToken.text != "if") return statement
		cursor.eat()
		val expression = parseExpression()
		return ConditionalStatementNode(statement, ifToken, expression)
	}

	fun parsePassStatement(): PassNode {
		val passToken = cursor.expect { it.text == "pass" }
		return PassNode(passToken)
	}

	fun parseLabeledStatement(): LabeledLoopNode {
		val label = parseLabel()
		val statement = parseStatement()
		return LabeledLoopNode(label, statement)
	}

	fun parseStatement(): Statement {
		cursor.eatNL()
		val token = cursor.current ?: error("no current token")
		return when (token.text) {
			"if" -> parseConditionalGroup()
			"while" -> parseWhileLoop()
			"do" -> parseDoWhileLoop()
			"for" -> parseForLoop()
			"return" -> tryParseConditionalStatement(parseReturnStatement())
			"break" -> tryParseConditionalStatement(parseBreakStatement())
			"continue" -> tryParseConditionalStatement(parseContinueStatement())
			"pass" -> parsePassStatement()
			"/" -> parseLabeledStatement()
			else -> tryParseConditionalStatement(parseExpression())
		}
	}

	internal fun parsePossibleNamedArgumentList(terminatorText: String): List<Expression> {
		val arguments = mutableListOf<Pair<Pair<Token, Token>?, Expression>>()
		while (cursor.current?.text != terminatorText) {
			var pair: Pair<Token, Token>? = null
			if (cursor.current?.tokenKind == TokenKinds.Identifier && cursor.next?.text == "=") {
				pair = cursor.eat()!! to cursor.eat()!!
			}
			val expression = parsePrecedence(201) // we don't want tuples
			arguments += pair to expression
			cursor.eatNL()
			cursor.eatIf { it.text == "," }
			cursor.eatNL()
		}
		return arguments.map {
			if (it.first == null) it.second else NamedArgumentNode(
				IdentifierNode(it.first!!.first),
				it.first!!.second,
				it.second
			)
		}
	}

	// this function assumes that the first < token has been consumed already.
	internal fun parseGenerics(): List<TypeExpression> {
		val generics = mutableListOf<TypeExpression>()
		while (cursor.hasNext()) {
			val type = parseTypeExpression()
			generics += type
			if (cursor.current?.text == ">") {
				break
			}
			cursor.expect { it.text == "," }
		}
		return generics
	}

	fun parseParameter(): Parameter {
		val name = IdentifierNode(cursor.expect { it.tokenKind == TokenKinds.Identifier })
		val type = if (cursor.current?.text == ":") {
			cursor.eat()
			parseTypeExpression()
		} else {
			null
		}
		val default = if (cursor.current?.text == "=") {
			cursor.eat()
			parseExpression()
		} else {
			null
		}
		return Parameter(name, type, default)
	}

	fun parseParameters(): List<Parameter> {
		val parameters = mutableListOf<Parameter>()
		while (cursor.hasNext() && cursor.current!!.tokenKind == TokenKinds.Identifier) {
			parameters += parseParameter()
			if (cursor.current?.text == ",") {
				cursor.eat()
			} else {
				break
			}
		}
		return parameters
	}

	fun parseExplicitLambda(): ExplicitLambdaNode {
		if (cursor.current?.text == "(") {
			val leftParen = cursor.eat()!!
			val identifiers = parseParameters()
			val rightParen = cursor.expect { it.text == ")" }
			val arrowToken = cursor.expect { it.text == "->" }
			val body = parseBlock()
			return ExplicitLambdaNode(leftParen, identifiers, rightParen, arrowToken, body)
		}
		val identifiers = parseParameters()
		val arrowToken = cursor.expect { it.text == "->" }
		val body = parseBlock()
		return ExplicitLambdaNode(null, identifiers, null, arrowToken, body)
	}
}

fun TokenCursor.eatNL() = eatWhile {
	it.tokenKind == TokenKinds.Newline
}