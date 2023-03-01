package dev.kason.mono.compiler.lexer

import dev.kason.mono.compiler.base.CodeIndex
import dev.kason.mono.compiler.error.CodeAnnotation
import dev.kason.mono.compiler.error.ErrorContext
import dev.kason.mono.compiler.error.ErrorReceiver
import dev.kason.mono.compiler.error.ErrorType
import dev.kason.mono.compiler.lexer.UnterminatedSingleQuoteStringLiteralReceiver.appendHint

object UnterminatedSingleQuoteStringLiteralReceiver :
	ErrorReceiver<UnterminatedSingleQuoteStringLiteralContext>(1, ErrorType.LEXICAL) {
	override fun format(context: UnterminatedSingleQuoteStringLiteralContext): String = buildString {
		appendHeaders(context)
		appendIndex(context.indexOfStart)
		appendCodeLine(context.indexOfStart) {
			+CodeAnnotation(context.indexOfStart, "start of string literal")
		}
		appendLine("the string literal is never terminated")
		appendLine()
		appendHint(
			"single-quote string literals cannot span multiple lines",
			"if you intended to have the string literal span multiple lines, use a double-quote string literal instead."
		)
		appendHint("you may have forgotten to close the string literal with a single quote ('). ")
	}
}

class UnterminatedSingleQuoteStringLiteralContext(
	val indexOfStart: CodeIndex
) : ErrorContext {
	override val code: Int = 1
	override val simpleMessage: String = "unterminated single-quoted string literal"
}

object UnterminatedDoubleQuoteStringLiteralReceiver :
	ErrorReceiver<UnterminatedDoubleQuoteStringLiteralContext>(2, ErrorType.LEXICAL) {
	override fun format(context: UnterminatedDoubleQuoteStringLiteralContext): String = buildString {
		appendHeaders(context)
		appendIndex(context.indexOfStart)
		appendCodeLine(context.indexOfStart) {
			+CodeAnnotation(context.indexOfStart, "start of string literal")
		}
		appendLine("the string literal is never terminated")
		appendHint("you may have forgotten to close the string literal with a single quote (\"). ")
	}
}

class UnterminatedDoubleQuoteStringLiteralContext(
	val indexOfStart: CodeIndex
) : ErrorContext {
	override val code: Int = 2
	override val simpleMessage: String = "unterminated double-quoted string literal"
}