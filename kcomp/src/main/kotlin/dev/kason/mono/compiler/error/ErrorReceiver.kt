package dev.kason.mono.compiler.error

import dev.kason.mono.compiler.base.CodeIndex
import dev.kason.mono.compiler.base.CodeRange

enum class ErrorType {
	LEXICAL,
	SYNTAX,
	SEMANTIC,
	CODEGEN,
	INTERNAL;

	override fun toString(): String = name.lowercase()
}

// marker interface
interface ErrorContext {
	val code: Int
	val simpleMessage: String
}

abstract class ErrorReceiver<out ContextType : ErrorContext>(
	val code: Int,
	val type: ErrorType
) {
	abstract fun format(context: @UnsafeVariance ContextType): String

	protected fun StringBuilder.appendHeaders(context: @UnsafeVariance ContextType) =
		appendLine("error [c${code.toString().padStart(4, '0')}]: ${context.simpleMessage}")

	protected fun StringBuilder.appendCodeLine(codeIndex: CodeIndex, block: AnnotatedCodeBlock.() -> Unit) {
		val codeLine = codeIndex.line
		val codeBlock = AnnotatedCodeBlock(codeIndex.source, codeLine, codeLine)
		codeBlock.block()
		append(codeBlock)
	}

	protected fun StringBuilder.appendRange(codeRange: CodeRange) =
		append("--> ").appendLine(codeRange)

	protected fun StringBuilder.appendIndex(codeIndex: CodeIndex) =
		append("--> ").appendLine(codeIndex)

	protected var hintNumber = 1

	protected fun StringBuilder.appendHint(vararg lines: String): StringBuilder {
		val hintString = (hintNumber++).toString().padStart(3) + " ): "
		append(hintString)
		for (line in lines.indices) {
			if (line != 0) {
				appendLine()
				append(" ".repeat(hintString.length))
			}
			append(lines[line])
		}
		return this
	}

	protected fun StringBuilder.appendCodeLine(codeRange: CodeRange, block: AnnotatedCodeBlock.() -> Unit) {
		val codeBlock = AnnotatedCodeBlock(codeRange)
		codeBlock.block()
		append(codeBlock)
	}
}

