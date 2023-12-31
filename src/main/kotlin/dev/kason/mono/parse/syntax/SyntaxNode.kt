package dev.kason.mono.parse.syntax

import dev.kason.mono.core.IndexRange
import dev.kason.mono.core.rangeTo
import dev.kason.mono.core.read
import dev.kason.mono.parse.lexing.Token
import dev.kason.mono.parse.lexing.escape

abstract class SyntaxNode(val range: IndexRange) {
	var parent: SyntaxNode? = null
		private set

	constructor(vararg nodes: SyntaxNode?) : this(IndexRange(nodes.mapNotNull { it?.range })) {
		for (node in nodes) {
			if (node != null) plusAssign(node)
		}
	}

	// transfer nodes from this node to the parent
	internal fun removeParent() {
		this.parent = null
	}

	val children: MutableList<SyntaxNode> = mutableListOf()
	val text: String = range.read()

	operator fun plusAssign(syntaxNode: SyntaxNode) {
		if (syntaxNode.parent != null) {
			error("node $syntaxNode already has parent, cannot assign to $this")
		}
		syntaxNode.parent = this
		this.children += syntaxNode
	}

	operator fun plusAssign(syntaxNodes: Iterable<SyntaxNode>) {
		for (syntaxNode in syntaxNodes) {
			this += syntaxNode
		}
	}

	operator fun get(index: Int): SyntaxNode = this.children[index]
}

// class providing utility functions for syntax trees
class SyntaxTree(val root: SyntaxNode) {
	val count: Int
		get() = nodes.size

	val nodes: List<SyntaxNode>
		get() {
			val list = mutableListOf<SyntaxNode>()
			visit { list.add(it) }
			return list
		}

	fun visit(visitor: (SyntaxNode) -> Unit) {
		val queue = ArrayDeque<SyntaxNode>()
		queue.add(root)
		while (queue.isNotEmpty()) {
			val item = queue.removeFirst()
			visitor(item)
			for (child in item.children) {
				queue.add(child)
			}
		}
	}
}

open class Statement(range: IndexRange) : SyntaxNode(range)

open class Expression(range: IndexRange) : Statement(range) {
	constructor(vararg nodes: SyntaxNode) : this(IndexRange(nodes.map { it.range })) {
		for (node in nodes) {
			plusAssign(node)
		}
	}
}

class BlockNode(
	val statements: List<Statement>
) : SyntaxNode(IndexRange(statements.map { it.range })) {
	init {
		this += statements
	}

	override fun toString(): String = "block '${statements.joinToString("\n")}'"
}

class ConditionalGroupNode(
	val conditionals: List<ConditionalNode>
) : Expression(*conditionals.toTypedArray()) {

	override fun toString(): String = "conditional group '${conditionals.joinToString("\n")}'"
}

class ConditionalNode(
	val keywordToken: Token,
	val condition: Expression?,
	val body: BlockNode
) : Statement(keywordToken.range..body.range) {
	init {
		if (condition != null) this += condition
		this += body
	}

	override fun toString(): String = "if $condition: $body"
}

class WhileLoopNode(
	val whileToken: Token,
	val condition: Expression,
	val body: BlockNode
) : Statement(whileToken.range..body.range) {
	init {
		this += condition
		this += body
	}

	override fun toString(): String = "while $condition: $body"
}

class DoWhileLoopNode(
	val doToken: Token,
	val body: BlockNode,
	val whileToken: Token,
	val condition: Expression
) : Statement(doToken.range..condition.range) {
	init {
		this += body
		this += condition
	}

	override fun toString(): String = "do $body while $condition"
}

class ForLoopNode(
	val forToken: Token,
	val identifiers: List<IdentifierNode>,
	val inToken: Token,
	val expression: Expression,
	val body: BlockNode
) : Statement(forToken.range..body.range) {
	init {
		this += identifiers
		this += expression
		this += body
	}

	override fun toString(): String = "for ${identifiers.joinToString(", ")} in $expression: $body"
}

class LabeledLoopNode(
	val label: Label,
	val loop: Statement
) : Statement(label.range..loop.range) {
	init {
		this += label
		this += loop
	}

	override fun toString(): String = "$label: $loop"
}

fun labeledTokenRange(token: Token, possibleLabel: Label?): IndexRange {
	if (possibleLabel != null) {
		return token.range..possibleLabel.range
	}
	return token.range
}

class BreakNode(
	val breakToken: Token,
	val label: Label?
) : Statement(labeledTokenRange(breakToken, label)) {
	init {
		if (label != null) this += label
	}

	override fun toString(): String = "break"
}

class ContinueNode(
	val continueToken: Token,
	val label: Label?
) : Statement(labeledTokenRange(continueToken, label)) {

	init {
		if (label != null) this += label
	}

	override fun toString(): String = "continue"
}

class Label(
	val leftSlash: Token,
	val name: IdentifierNode,
	val rightSlash: Token
) : SyntaxNode(leftSlash.range..rightSlash.range) {
	init {
		this += name
	}

	override fun toString(): String = "/$name/"
}

class ReturnNode(
	val returnToken: Token,
	val label: Label?,
	val expression: Expression
) : Statement(returnToken.range..expression.range) {
	init {
		this += expression
		if (label != null) this += label
	}

	override fun toString(): String = "return $expression"
}

class PassNode(val passToken: Token) : Statement(passToken.range) {
	override fun toString(): String = "pass"
}

class ConditionalStatementNode(
	val action: Statement,
	val ifToken: Token,
	val expression: Expression
) : Statement(action.range..expression.range) {
	init {
		this += action
		this += expression
	}

	override fun toString(): String = "$action if $expression"
}

class NamedArgumentNode(
	val name: IdentifierNode,
	val equalsToken: Token,
	val value: Expression
) : Expression(name.range..value.range) {
	init {
		this += name
		this += value
	}

	override fun toString(): String = "$name = $value"
}

data class Parameter(
	val name: IdentifierNode,
	val type: TypeExpression?,
	val default: Expression?
) : SyntaxNode(name, type, default) {
	override fun toString(): String = "$name: $type = $default"
}

// a, b -> c
// (a, b) -> c
// () -> c
// (a: int, b) -> c
class ExplicitLambdaNode(
	val leftParen: Token?,
	val identifiers: List<Parameter>,
	val rightParen: Token?,
	val arrowToken: Token,
	val body: BlockNode
) : Expression((leftParen?.range ?: identifiers.first().range)..body.range) {
	init {
		this += identifiers
		this += body
	}

	override fun toString(): String = "(${identifiers.joinToString(", ")}) -> $body"
}
