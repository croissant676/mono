package dev.kason.mono.compiler.base

private const val UNINITIALIZED_PROPERTY_VALUE = -1

class CodeIndex(
	val source: CodeSource,
	val index: Int
) : Comparable<CodeIndex> {
	private var initialized: Boolean = false
	private var internalLine = UNINITIALIZED_PROPERTY_VALUE
	private var internalColumn = UNINITIALIZED_PROPERTY_VALUE

	val line: Int
		get() = initializeIfNot() and internalLine

	val column: Int
		get() = initializeIfNot() and internalColumn

	constructor(source: CodeSource, line: Int, column: Int)
		: this(source, source.findIndex(line, column)) {
		initialized = true
		internalLine = line
		internalColumn = column
	}

	// for internal use only
	internal constructor(source: CodeSource, index: Int, line: Int, column: Int)
		: this(source, index) {
		initialized = true
		internalLine = line
		internalColumn = column
	}

	private fun initializeIfNot(): Int {
		if (!initialized) {
			val (line, column) = source.lineAndColumnAt(index)
			internalLine = line
			internalColumn = column
			initialized = true
		}
		return UNINITIALIZED_PROPERTY_VALUE
	}

	override fun compareTo(other: CodeIndex): Int {
		if (source != other.source) {
			throw IllegalArgumentException("cannot compare indices from different sources")
		}
		return index - other.index
	}

	operator fun plus(offset: Int): CodeIndex = CodeIndex(source, index + offset)
	operator fun minus(offset: Int): CodeIndex = CodeIndex(source, index - offset)

	operator fun rangeTo(other: CodeIndex): CodeRange {
		if (source != other.source) {
			throw IllegalArgumentException("cannot create range from indices from different sources")
		}
		return CodeRange(source, index, other.index)
	}

	operator fun component0(): Int = line
	operator fun component1(): Int = column

	override fun hashCode(): Int = index * 31 + source.hashCode()
	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is CodeIndex) return false
		return index == other.index && source == other.source
	}

	override fun toString(): String = "${source.name}  ($line:$column)"

}


// starting from start inclusive and ending at end exclusive
class CodeRange(
	val source: CodeSource,
	val startIndex: Int,
	val endIndex: Int
) : ClosedRange<CodeIndex>, Iterable<CodeIndex> {
	private var initialized: Boolean = false
	private var internalStartLine = UNINITIALIZED_PROPERTY_VALUE
	private var internalStartColumn = UNINITIALIZED_PROPERTY_VALUE
	private var internalEndLine = UNINITIALIZED_PROPERTY_VALUE
	private var internalEndColumn = UNINITIALIZED_PROPERTY_VALUE

	constructor(source: CodeSource, start: CodeIndex, end: CodeIndex)
		: this(source, start.index, end.index) {
		initialized = true
		internalStartLine = start.line
		internalStartColumn = start.column
		internalEndLine = end.line
		internalEndColumn = end.column
	}

	val startLine: Int
		get() = initializeIfNot() and internalStartLine

	val startColumn: Int
		get() = initializeIfNot() and internalStartColumn

	val endLine: Int
		get() = initializeIfNot() and internalEndLine

	val endColumn: Int
		get() = initializeIfNot() and internalEndColumn

	override val start: CodeIndex
		get() = CodeIndex(source, startIndex, startLine, startColumn)

	val end: CodeIndex
		get() = CodeIndex(source, endIndex, endLine, endColumn)

	override val endInclusive: CodeIndex
		get() = end - 1

	val length: Int
		get() = endIndex - startIndex

	private fun initializeIfNot(): Int {
		if (!initialized) {
			val (startLine, startColumn) = source.lineAndColumnAt(startIndex)
			val (endLine, endColumn) = source.lineAndColumnAt(endIndex)
			internalStartLine = startLine
			internalStartColumn = startColumn
			internalEndLine = endLine
			internalEndColumn = endColumn
			initialized = true
		}
		return UNINITIALIZED_PROPERTY_VALUE
	}

	override fun iterator(): Iterator<CodeIndex> = object : Iterator<CodeIndex> {
		var index = startIndex
		override fun hasNext(): Boolean = index < endIndex
		override fun next(): CodeIndex = CodeIndex(source, index++)
	}

	override fun toString(): String {
		if (startIndex == endIndex) {
			return start.toString()
		}
		if (startLine == endLine) {
			return "${source.name}  ($startLine:$startColumn - $endColumn)"
		}
		return "${source.name} ($startLine:$startColumn - $endLine:$endColumn)"
	}

	operator fun component0(): CodeIndex = start
	operator fun component1(): CodeIndex = end

	override fun hashCode(): Int {
		var result = source.hashCode()
		result = 31 * result + startIndex
		result = 31 * result + endIndex
		return result
	}

	override fun equals(other: Any?): Boolean {
		if (this === other) return true
		if (other !is CodeRange) return false
		return source == other.source && startIndex == other.startIndex && endIndex == other.endIndex
	}
}


fun CodeIndex.asSingleRange(): CodeRange = CodeRange(source, index, index + 1)
fun CodeIndex.lineStart(): CodeIndex = source.lineStartAt(index)
fun CodeIndex.lineEnd(): CodeIndex = source.lineEndAt(index)

fun CodeIndex.charValue(): Char = source.content[index]
fun CodeIndex.line(): String = source.line(line)
fun CodeIndex.isValidIndex(): Boolean = index in source.content.indices

val CodeRange.isSingleLine: Boolean
	get() = startLine == endLine

val CodeRange.lineCount: Int
	get() = endLine - startLine + 1

fun CodeRange.completed(): CodeRange {
	val start = source.lineStartAt(startIndex)
	val end = source.lineEndAt(endIndex)
	return CodeRange(source, start.index, end.index)
}

fun CodeRange.asSingleIndex(): CodeIndex {
	if (startIndex != endIndex) {
		throw IllegalArgumentException("cannot convert range to index")
	}
	return CodeIndex(source, startIndex)
}

fun CodeRange.content(): String = source.content.substring(startIndex, endIndex)
fun CodeRange.lines(): List<String> = source.lines(startLine, endLine)
fun CodeRange.completedLines(): List<String> = completed().lines()

fun CodeRange.lineRange(): IntRange = startLine..endLine