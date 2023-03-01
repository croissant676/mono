package dev.kason.mono.compiler.error

import dev.kason.mono.compiler.base.CodeRange
import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.base.isSingleLine

class AnnotatedCodeBlock(
	val source: CodeSource,
	val startLine: Int,
	val endLine: Int
) {
	val lines = source.lines(startLine, endLine)
	// todo add support for tabs
	val annotations = mutableListOf<CodeAnnotation>()

	init {
		if (startLine > endLine) {
			throw IllegalArgumentException("start line cannot be greater than end line")
		}
	}

	constructor(range: CodeRange) : this(range.source, range.start.line, range.end.line)

	fun StringBuilder.appendLine(index: Int, padTo: Int) {
		val lineNumber = startLine + index
		val line = lines[index]
		append(lineNumber.toString().padStart(padTo))
		append(" | ")
		append(line)
		appendLine()
		annotations.filter { it.range.start.line == lineNumber }.forEach {
			append(" ".repeat(padTo))
			append(" | ")
			for (lineIndex in 0 until it.range.startColumn) {
				if (line[lineIndex] == '\t') {
					append('\t')
				} else {
					append(' ')
				}
			}
			for (lineIndex in it.range.startColumn until it.range.endColumn) {
				if (line[lineIndex] == '\t') {
					append('\t')
				} else {
					append('^')
				}
			}
			append(" ")
			append(it.message)
			appendLine()
		}
	}

	override fun toString(): String = buildString {
		val padTo = endLine.toString().length
		lines.indices.forEach {
			appendLine(it, padTo)
		}
	}
}

data class CodeAnnotation(
	val range: CodeRange,
	val message: String
) {
	init {
		if (!range.isSingleLine) {
			throw IllegalArgumentException("range must be a single line")
		}
	}
}