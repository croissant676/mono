package dev.kason.mono.compiler.error

import dev.kason.mono.compiler.base.CodeIndex
import dev.kason.mono.compiler.base.CodeRange
import dev.kason.mono.compiler.base.line

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
		append("error [c${code.toString().padStart(4, '0')}]: ${context.simpleMessage}")!!

	protected fun StringBuilder.appendCodeLine(codeIndex: CodeIndex, block: AnnotatedCodeBlock.() -> Unit) {
		val codeLine = codeIndex.line
		val codeBlock = AnnotatedCodeBlock(codeIndex.source, codeLine, codeLine)
		codeBlock.block()
		append(codeBlock)
	}


	protected fun StringBuilder.appendCodeLine(codeRange: CodeRange, block: AnnotatedCodeBlock.() -> Unit) {
		val codeBlock = AnnotatedCodeBlock(codeRange)
		codeBlock.block()
		append(codeBlock)
	}
}

