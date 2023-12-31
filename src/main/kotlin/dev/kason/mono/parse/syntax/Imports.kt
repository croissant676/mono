package dev.kason.mono.parse.syntax

import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.rangeTo
import dev.kason.mono.parse.lexing.Token
import dev.kason.mono.parse.lexing.TokenKinds

// parse import statements that look like
// import mono:io[File, web[Request, Response], tcp[Socket], Serialization as Ser]

abstract class ImportNode(range: IndexRange) : SyntaxNode(range)

class ImportStarNode(val token: Token) : ImportNode(token.range) {
	override fun toString(): String = "ImportStarNode($text)"
}

class ImportIdentifierNode(val token: Token) : ImportNode(token.range) {
	override fun toString(): String = "ImportIdentifierNode($text)"
}

class ImportAsNode(
	val importIdentifierNode: ImportNode,
	val asToken: Token,
	val name: Token
) : ImportNode(importIdentifierNode.range..name.range) {

	init {
		this += importIdentifierNode
	}

	override fun toString(): String = "ImportAsNode($name)"
}

object ImportAsParselet : InfixParselet<ImportNode> {
	override val precedence: Int get() = 1000

	override fun parse(syntaxParser: SyntaxParser, left: ImportNode, token: Token): ImportNode {
		val name = syntaxParser.cursor.expect { it.tokenKind == TokenKinds.Identifier }
		return ImportAsNode(left, token, name)
	}
}

class ImportMultiNode(
	val importIdentifierNode: ImportIdentifierNode,
	val leftBracket: Token,
	val imports: ImportNode,
	val rightBracket: Token
) : ImportNode(importIdentifierNode.range..rightBracket.range) {

	init {
		this += importIdentifierNode
		this += imports
	}

	override fun toString(): String = "ImportMultiNode($imports)"
}

object ImportMultiParselet : InfixParselet<ImportNode> {
	override val precedence: Int get() = 1000

	override fun parse(syntaxParser: SyntaxParser, left: ImportNode, token: Token): ImportNode {
		val imports = syntaxParser.parseImport()
		val rightBracket = syntaxParser.cursor.expect { it.text == "]" }
		return ImportMultiNode(left as ImportIdentifierNode, token, imports, rightBracket)
	}
}

class ImportTupleNode(
	val left: ImportNode,
	val comma: Token,
	val right: ImportNode
) : ImportNode(left.range..right.range) {
	init {
		this += left
		this += right
	}

	override fun toString(): String = "ImportTupleNode($left, $right)"
}

object ImportTupleParselet : InfixParselet<ImportNode> {
	override val precedence: Int get() = 1000

	override fun parse(syntaxParser: SyntaxParser, left: ImportNode, token: Token): ImportNode {
		val right = syntaxParser.parseImport()
		return ImportTupleNode(left, token, right)
	}
}

class ImportQualified(
	val left: ImportNode,
	val dot: Token,
	val right: ImportNode
) : ImportNode(left.range..right.range) {

	init {
		this += left
		this += right
	}

	override fun toString(): String = "ImportQualified($left, $right)"
}

object ImportQualifiedParselet : InfixParselet<ImportNode> {
	override val precedence: Int get() = 1000

	override fun parse(syntaxParser: SyntaxParser, left: ImportNode, token: Token): ImportNode {
		val right = syntaxParser.parseImport()
		return ImportQualified(left, token, right)
	}
}

val ImportParseletRegistry = DynamicParseletRegistry<InfixParselet<ImportNode>>().apply {
	this on { it.text == "as" } use ImportAsParselet
	this on { it.text == "," } use ImportTupleParselet
	this on { it.text == "[" } use ImportMultiParselet
	this on { it.text == "." } use ImportQualifiedParselet
}

fun SyntaxParser.parseImport(): ImportNode {
	val current = cursor.current ?: error("unexpected end of file")
	if (current.text == "*") {
		val starToken = cursor.eat() ?: error("no current token")
		return ImportStarNode(starToken)
	}
	val identifier = cursor.expect { it.tokenKind == TokenKinds.Identifier }
	val importNode: ImportNode = ImportIdentifierNode(identifier)
	return parseInfix(importNode, ImportParseletRegistry, 0)
}


class ImportStatementNode(
	val importToken: Token,
	val module: IdentifierNode?,
	val import: ImportNode
) : Statement(importToken.range..import.range) {
	init {
		this += import
	}

	override fun toString(): String = "ImportStatementNode($import)"
}

fun SyntaxParser.parseImportStatement(): ImportStatementNode {
	cursor.eatNL()
	val importToken = cursor.expect { it.text == "import" }
	// could start with <module>:<import node>
	return cursor.transaction {
		val module = cursor.eatIf { it.tokenKind == TokenKinds.Identifier }
		cursor.expect { it.text == ":" }
		val import = parseImport()
		return@transaction ImportStatementNode(importToken, IdentifierNode(module!!), import)
	} ?: ImportStatementNode(importToken, null, parseImport())
}