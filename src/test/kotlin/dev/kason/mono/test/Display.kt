package dev.kason.mono.test

import dev.kason.mono.parse.syntax.SyntaxNode
import hu.webarticum.treeprinter.TreeNode
import hu.webarticum.treeprinter.decorator.BorderTreeNodeDecorator
import hu.webarticum.treeprinter.printer.traditional.TraditionalTreePrinter
import hu.webarticum.treeprinter.text.AnsiConsoleText
import hu.webarticum.treeprinter.text.ConsoleText

class TreeNodeImpl(val syntaxNode: SyntaxNode): TreeNode {
	override fun content(): ConsoleText =
		AnsiConsoleText(syntaxNode::class.simpleName + "\n" + syntaxNode.text)

	override fun children(): List<TreeNode> =
		syntaxNode.children.map { TreeNodeImpl(it) }
}

val treePrinter = TraditionalTreePrinter()

fun printTree(syntaxNode: SyntaxNode) =
	treePrinter.print(BorderTreeNodeDecorator(TreeNodeImpl(syntaxNode)))