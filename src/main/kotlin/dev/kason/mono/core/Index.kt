package dev.kason.mono.core

// helper function to shorten functions to single lines:
infix fun <T> Unit.then(item: T): T = item

private fun checkSameDocument(first: Document, second: Document, block: () -> String) =
	require(first == second) {
		"${block()} ($first, $second)"
	}

internal const val UNINITIALIZED = -1

class Index(val document: Document, val index: Int) : Comparable<Index> {

	private var internalLineNumber: Int = UNINITIALIZED
	private var internalColumn: Int = UNINITIALIZED

	init {
		document.rangeCheck(index)
	}

	constructor(document: Document, line: Int, column: Int) : this(document, document.getIndexFor(line, column)) {
		internalLineNumber = line
		internalColumn = column
	}

	// we don't check, assume caller has correct info
	internal constructor(document: Document, line: Int, column: Int, index: Int) : this(document, index) {
		internalLineNumber = line
		internalColumn = column
	}

	val isInitialized: Boolean
		get() = internalLineNumber != UNINITIALIZED && internalColumn != UNINITIALIZED

	fun initialize() {
		if (isInitialized) return
		val (line, column) = document.getLineAndColumnFor(index)
		internalLineNumber = line
		internalColumn = column
	}

	val lineNumber: Int get() = initialize() then internalLineNumber
	val column: Int get() = initialize() then internalColumn

	override fun compareTo(other: Index): Int {
		checkSameDocument(document, other.document) { "can't compare indices from different documents" }
		return index.compareTo(other.index)
	}

	operator fun plus(offset: Int): Index = Index(document, index + offset)
	operator fun minus(offset: Int): Index = Index(document, index - offset)

	/** distance from other index (this - index). can be negative, use abs() to find absolute distance */
	fun distanceTo(other: Index): Int {
		checkSameDocument(document, other.document) { "can't measure index distance from different documents" }
		return index - other.index
	}

	operator fun minus(other: Index): Int = distanceTo(other)

	operator fun rangeTo(other: Index): IndexRange = IndexRange(this, other)
	operator fun rangeUntil(other: Index): IndexRange = IndexRange(this, other - 1)

	operator fun rangeTo(other: Int): IndexRange = rangeTo(document.index(other))
	operator fun rangeUntil(other: Int): IndexRange = rangeTo(other - 1)

	override fun equals(other: Any?): Boolean =
		other is Index && other.document == document && other.index == index

	override fun hashCode(): Int = document.hashCode() * 31 + index
	override fun toString(): String =
		"${document.source.name} (line $lineNumber, column $column)"

}

class IndexRange(val document: Document, val startIndex: Int, val endIndex: Int) : ClosedRange<Index>, Iterable<Index> {

	private var internalStartLine: Int = UNINITIALIZED
	private var internalStartColumn: Int = UNINITIALIZED
	private var internalEndLine: Int = UNINITIALIZED
	private var internalEndColumn: Int = UNINITIALIZED

	val length: Int get() = endIndex - startIndex + 1

	init {
		document.rangeCheck(startIndex)
		document.rangeCheck(endIndex)
	}

	constructor(start: Index, end: Index) : this(start.document, start.index, end.index) {
		checkSameDocument(start.document, end.document) { "can't create range from indices in different documents" }
		internalStartLine = start.lineNumber
		internalStartColumn = start.column
		internalEndLine = end.lineNumber
		internalEndColumn = end.column
	}

	val isInitialized: Boolean
		get() = internalStartLine != UNINITIALIZED &&
			internalStartColumn != UNINITIALIZED &&
			internalEndLine != UNINITIALIZED &&
			internalEndColumn != UNINITIALIZED

	fun initialize() {
		if (isInitialized) return
		val (startLine, startColumn) = document.getLineAndColumnFor(startIndex)
		val (endLine, endColumn) = document.getLineAndColumnFor(endIndex)
		internalStartLine = startLine
		internalStartColumn = startColumn
		internalEndLine = endLine
		internalEndColumn = endColumn
	}

	val startLine: Int get() = initialize() then internalStartLine
	val startColumn: Int get() = initialize() then internalStartColumn
	val endLine: Int get() = initialize() then internalEndLine
	val endColumn: Int get() = initialize() then internalEndColumn

	override val start: Index get() = Index(document, startIndex, startLine, startColumn)
	override val endInclusive: Index get() = Index(document, endIndex, endLine, endColumn)

	override fun iterator(): Iterator<Index> = object : Iterator<Index> {
		private var index = startIndex
		override fun hasNext(): Boolean = index <= endIndex
		override fun next(): Index = Index(document, index++)
	}

	override fun toString(): String = when {
		isEmpty() -> "${document.source.name} (empty range)"
		length == 1 -> "${document.source.name} (line $startLine, column $startColumn)"
		startLine == endLine -> "${document.source.name} (line $startLine, columns $startColumn - $endColumn)"
		else -> "${document.source.name} (line $startLine, column $startColumn - line $endLine, column $endColumn)"
	}
}

fun Index.toRange(): IndexRange = this.rangeTo(this)
fun Index.read(): Char = document[index]

fun Index.rangeFromOffset(offset: Int): IndexRange = this.rangeTo(this + offset)

val IndexRange.lineCount: Int get() = endInclusive.lineNumber - start.lineNumber + 1
val IndexRange.isSingleLine: Boolean get() = startLine == endLine

fun IndexRange.read(): String = if (isEmpty()) "" else document.substring(startIndex, endIndex + 1)

val Index.line: Line get() = document.lines[lineNumber]
val IndexRange.lineNumbers: IntRange get() = startLine..endLine
val IndexRange.lines: List<Line> get() = document.lines[lineNumbers]

