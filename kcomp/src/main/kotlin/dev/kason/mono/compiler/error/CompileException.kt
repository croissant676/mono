package dev.kason.mono.compiler.error

import dev.kason.mono.compiler.lexer.UnterminatedDoubleQuoteStringLiteralReceiver
import dev.kason.mono.compiler.lexer.UnterminatedSingleQuoteStringLiteralReceiver
import kotlin.system.exitProcess

class CompileException(val context: ErrorContext) : Throwable("compiler exception", null)

object ErrorManager {
	val receivers: List<ErrorReceiver<*>> = listOf(
		InternalErrorReceiver,
		UnterminatedSingleQuoteStringLiteralReceiver,
		UnterminatedDoubleQuoteStringLiteralReceiver
	)

	fun report(exception: CompileException) {
		val receiver = (receivers.getOrNull(exception.context.code)
			?: throw RuntimeException("no receiver for error code ${exception.context.code}", exception)) as ErrorReceiver<ErrorContext>
		System.err.println(receiver.format(exception.context))
		System.err.println("error: aborting due to error, terminating compilation.")
		exitProcess(receiver.code + 1)
	}

	fun executeSafe(block: () -> Unit) {
		try {
			block()
		} catch (compilerException: CompileException) {
			try {
				report(compilerException)
			} catch (throwable: Exception) {
				report(CompileException(InternalErrorContext(throwable)))
			}
		} catch (otherError: Throwable) {
			report(CompileException(InternalErrorContext(otherError)))
		}
	}
}
