package dev.kason.mono.compiler.base

import dev.kason.mono.compiler.module.CodeModule
import java.io.Reader

class CodeSource(
	val module: CodeModule,
	val name: String,
	val content: String
) {

	constructor(module: CodeModule, name: String, reader: Reader)
		: this(module, name, reader.readText())

	val newLineIndices by lazy {
		listOf(-1) + content.indices.filter { content[it] == '\n' } + content.length
	}
	val completeRange: CodeRange = CodeRange(this, 0, content.length)

	internal fun lineAndColumnAt(index: Int): Pair<Int, Int> {
		if (index < 0 || index > content.length) {
			throw IndexOutOfBoundsException("index $index is out of bounds")
		}
		val line = newLineIndices.binarySearch(index)
		val lineIndex = if (line >= 0) line else -line - 2
		val column = index - newLineIndices[lineIndex]
		return lineIndex + 1 to column
	}

	fun findIndex(line: Int, column: Int): Int {
		val lineIndex = line - 1
		if (lineIndex < 0 || lineIndex >= newLineIndices.size - 1) {
			throw IndexOutOfBoundsException("line $line is out of bounds")
		}
		val start = newLineIndices[lineIndex]
		val end = newLineIndices[lineIndex + 1]
		if (column < 0 || column > end - start) {
			throw IndexOutOfBoundsException("column $column is out of bounds")
		}
		return start + column
	}

	fun index(index: Int): CodeIndex = CodeIndex(this, index)

	fun index(line: Int, column: Int): CodeIndex = CodeIndex(this, line, column)
	fun range(start: CodeIndex, end: CodeIndex): CodeRange = CodeRange(this, start.index, end.index)

	fun range(start: Int, end: Int): CodeRange = CodeRange(this, start, end)

	fun lineStartAt(index: Int): CodeIndex {
		if (index < 0 || index > content.length) {
			throw IndexOutOfBoundsException("index $index is out of bounds")
		}
		val line = newLineIndices.binarySearch(index)
		val lineIndex = if (line >= 0) line else -line - 2
		return CodeIndex(this, newLineIndices[lineIndex] + 1)
	}

	fun lineEndAt(index: Int): CodeIndex {
		if (index < 0 || index > content.length) {
			throw IndexOutOfBoundsException("index $index is out of bounds")
		}
		val line = newLineIndices.binarySearch(index)
		val lineIndex = if (line >= 0) line else -line - 2
		return CodeIndex(this, newLineIndices[lineIndex + 1])
	}

	fun lineCount(): Int = newLineIndices.size - 1

	fun line(line: Int): String {
		val lineIndex = line - 1
		if (lineIndex < 0 || lineIndex >= lineCount()) {
			throw IndexOutOfBoundsException("line $line is out of bounds")
		}
		val start = newLineIndices[lineIndex] + 1
		val end = newLineIndices[lineIndex + 1]
		return content.substring(start, end)
	}

	fun lines(start: Int, end: Int): List<String> {
		val startLine = start - 1
		val endLine = end - 1
		if (startLine < 0 || startLine >= lineCount()) {
			throw IndexOutOfBoundsException("start line $start is out of bounds")
		}
		if (endLine < 0 || endLine >= lineCount()) {
			throw IndexOutOfBoundsException("end line $end is out of bounds")
		}
		return (startLine..endLine).map { line(it + 1) }
	}

	fun lines(): List<String> = lines(1, lineCount())

	override fun toString(): String = "CodeSource(module=$module, name='$name')"

}

