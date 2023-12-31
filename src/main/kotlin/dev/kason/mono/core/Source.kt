package dev.kason.mono.core

import java.io.File

// ---- Source ----

abstract class Source {
	abstract val name: String
	abstract val module: Module?

	protected abstract fun read(): String
	protected abstract fun shouldUpdate(): Boolean

	private var previous: Document? = null
	val document: Document
		get() {
			if (previous == null || shouldUpdate()) {
				previous = Document(this, sanitize(read()))
			}
			return previous!!
		}

	override fun toString(): String =
		"source '$name'${if (module != null) " from $module" else ""}"
}

private fun sanitize(content: String) = content.replace("\r\n", "\n")
	.replace("\r", "\n")

class StringSource(
	private val text: String,
	override val name: String,
	override val module: Module? = null
) : Source() {
	constructor(name: String, module: Module? = null, block: () -> String) : this(block(), name, module)

	override fun read(): String = text
	override fun shouldUpdate(): Boolean = false
	override fun equals(other: Any?): Boolean =
		other is StringSource && other.text == text && other.name == name

	override fun hashCode(): Int = text.hashCode()
}

class FileSource(val file: File, override val module: Module? = null) : Source() {

	constructor(path: String, module: Module? = null) : this(File(path), module)

	override val name: String = file.path
	private var lastRead: Long = 0

	override fun read(): String = file.readText()
		.also { lastRead = System.currentTimeMillis() }

	override fun shouldUpdate(): Boolean = lastRead < file.lastModified()
	override fun equals(other: Any?): Boolean =
		other is FileSource && other.file == file

	override fun hashCode(): Int = file.hashCode()

	override fun toString(): String =
		"file '$name'${if (module != null) " from $module" else ""}"
}

// ---- Document ----

private var documentCount = 0

class Document(val source: Source, val text: String) : CharSequence by text {
	val id: Int = documentCount++
	val newlineIndices: List<Int> = listOf(-1) + indices.filter { text[it] == '\n' } + length
	val lineCount = newlineIndices.size - 1

	val fullRange = range(0, if (isEmpty()) 0 else length - 1)
	val emptyRange = range(if (isEmpty()) 0 else 1, 0)

	internal fun rangeCheck(index: Int) = require(index in indices) {
		"index $index out of bounds for $this with length $length"
	}

	internal fun lineCheck(line: Int) =
		require(line in 1..lineCount) {
			"line $line out of bounds for $this with $lineCount lines"
		}

	fun getLineAndColumnFor(index: Int): Pair<Int, Int> {
		rangeCheck(index)
		val bsResult = newlineIndices.binarySearch(index)
		if (bsResult >= 0) return bsResult to (index - newlineIndices[bsResult - 1])
		val insertionPoint = -bsResult - 1
		return insertionPoint to (index - newlineIndices[insertionPoint - 1])
	}

	fun getIndexFor(line: Int, column: Int): Int {
		lineCheck(line)
		val result = getStartNL(line) + column
		require(column >= 1 && result <= newlineIndices[line]) {
			val lineLength = lineLength(line)
			"column $column out of bounds for line $line in $this with line length $lineLength"
		}
		return result
	}

	fun getStartNL(line: Int): Int = lineCheck(line) then newlineIndices[line - 1]
	fun getEndNL(line: Int): Int = lineCheck(line) then newlineIndices[line]

	// getStart() > getEnd() if line is empty
	fun getEnd(line: Int): Int = (getEndNL(line) - 1).coerceAtLeast(0)
	fun getStart(line: Int): Int = (getStartNL(line) + 1).coerceAtMost(lastIndex)
	fun lineLength(line: Int): Int = getEndNL(line) - getStart(line)

	fun readLine(line: Int): String = text.substring(getStart(line), getEndNL(line))

	fun readLines(startLine: Int, endLine: Int): List<String> {
		lineCheck(startLine)
		lineCheck(endLine)
		require(startLine <= endLine) {
			"startLine $startLine can't be greater than endLine $endLine ($this)"
		}
		return (startLine..endLine).map { readLine(it) }
	}

	override fun equals(other: Any?): Boolean =
		other is Document && other.id == id

	override fun hashCode(): Int = id
	override fun toString(): String = "doc $id; '${source.name}'"

	// lines functionality
	private val internalLineList: List<Line> = (1..lineCount).map { Line(this, it) }
	val lines = LineAccess()

	inner class LineAccess internal constructor() :
		Collection<Line> by internalLineList { // not a list because the index is 1-based
		override val size: Int get() = lineCount
		operator fun get(line: Int): Line = internalLineList[line - 1]
		operator fun get(range: IntRange): List<Line> = range.map(this::get)
		override fun isEmpty(): Boolean = lineCount == 0
		override fun iterator(): Iterator<Line> = internalLineList.iterator()
		override fun contains(element: Line): Boolean = element.document == this@Document
		override fun containsAll(elements: Collection<Line>): Boolean = elements.all(this::contains)
	}
}

class Line internal constructor(val document: Document, val number: Int) : CharSequence {

	val start: Index = document.index(document.getStart(number))
	val end: Index = document.index(document.getEnd(number))
	val startingNewline: Int = document.getStartNL(number)
	val endingNewline: Int = document.getEndNL(number)
	override val length: Int = document.lineLength(number)

	val range: IndexRange = start..end
	fun read(): String = document.readLine(number)

	fun rangeCheck(index: Int) = require(index in indices) {
		"index $index out of bounds for $this with length $length"
	}

	override fun get(index: Int): Char {
		require(index in indices) { "index $index out of bounds for $this with length $length" }
		return document[start.index + index]
	}

	override fun subSequence(startIndex: Int, endIndex: Int): String {
		rangeCheck(startIndex)
		rangeCheck(endIndex)
		require(startIndex <= endIndex) { "startIndex $startIndex can't be greater than endIndex $endIndex" }
		return document.substring(start.index + startIndex..start.index + endIndex)
	}

	override fun equals(other: Any?): Boolean =
		other is Line && other.document == document && other.number == number

	override fun hashCode(): Int = document.hashCode() * 31 + number
	override fun toString(): String = "line $number of $document"
}

fun Document.index(index: Int): Index = Index(this, index)
fun Document.index(line: Int, column: Int): Index = Index(this, line, column)

fun Document.range(start: Int, end: Int): IndexRange = IndexRange(this, start, end)
fun Document.range(range: IntRange): IndexRange = IndexRange(this, range.first, range.last)

// ---- Module ----

class Module
