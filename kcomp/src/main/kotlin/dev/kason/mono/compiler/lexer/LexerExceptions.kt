package dev.kason.mono.compiler.lexer

import dev.kason.mono.compiler.base.CodeIndex
import dev.kason.mono.compiler.error.*

object UnterminatedSingleQuoteStringLiteralReceiver :
	ErrorReceiver<UnterminatedSingleQuoteStringLiteralContext>(1, ErrorType.LEXICAL) {
	override fun format(context: UnterminatedSingleQuoteStringLiteralContext): String = buildString {
		appendHeaders(context)
		appendLine()
		appendCodeLine(context.indexOfStart) {
			+CodeAnnotation(context.indexOfStart, "start of string literal")
		}
		appendLine("the string literal is never terminated")
		appendLine("single quote literals cannot span multiple lines. if you intended to have a multi-line string, use double quotes instead.")
	}
}

class UnterminatedSingleQuoteStringLiteralContext(
	val indexOfStart: CodeIndex
) : ErrorContext {
	override val code: Int = 1
	override val simpleMessage: String = "unterminated single-quoted string literal (')"
}

object UnterminatedDoubleQuoteStringLiteralReceiver :
	ErrorReceiver<UnterminatedDoubleQuoteStringLiteralContext>(2, ErrorType.LEXICAL) {
	override fun format(context: UnterminatedDoubleQuoteStringLiteralContext): String = buildString {
		appendHeaders(context)
		appendLine()
		appendCodeLine(context.indexOfStart) {
			+CodeAnnotation(context.indexOfStart, "start of string literal")
		}
		appendLine("the string literal is never terminated")
		appendLine("you may have forgotten to close the string literal with a double quote (\"). ")
	}
}

class UnterminatedDoubleQuoteStringLiteralContext(
	val indexOfStart: CodeIndex
) : ErrorContext {
	override val code: Int = 2
	override val simpleMessage: String = "unterminated single-quoted string literal (\")"
}