package dev.kason.mono.compiler.error

object InternalErrorReceiver : ErrorReceiver<InternalErrorContext>(0, ErrorType.INTERNAL) {
	override fun format(context: InternalErrorContext): String = buildString {
		appendHeaders(context)
		appendLine()
		appendLine("the stack trace of the error is shown below. please report this error to the developers.")
		appendLine(context.error.formatPretty())
	}
}

class InternalErrorContext(
	val error: Throwable
) : ErrorContext {
	override val code: Int = 0
	override val simpleMessage: String = "internal error"
}

fun Throwable.formatPretty(): String {
	val stackTrace = this.stackTrace
	return buildString {
		// sample line:

		// (+) -- dev.kason.mono.compiler.error.InternalErrorReceiver.format (InternalErrorReceiver.kt:20)
		// with module
		// (+) -- dev.kason.mono.compiler.error.InternalErrorReceiver.format (InternalErrorReceiver.kt:20) [kcomp]
		//  +  -- java.lang.Integer.parseInt (Integer.java:615) [java.base]

		fun StackTraceElement.format(): String {
			val module: String? = this.moduleName
			val prefix = if (this.className.startsWith("dev.kason.mono.compiler")) "(+)" else " + "
			val classAndMethod = (this.className + '.' + this.methodName).replace("$", ".")
			val modulePart = if (module != null) "[$module]" else ""
			return "$prefix -- $classAndMethod (${this.fileName}:${this.lineNumber}) $modulePart"
		}

		// sample message:
		// internal error occurred while compiling (on thread "main"):
		//  *  -- java.lang.NumberFormatException: For input string: "dfd"
		//  +  -- dev.kason.mono.compiler.error.InternalErrorReceiver.format (InternalErrorReceiver.kt:20) [kcomp]
		//  +  -- dev.kason.mono.compiler.error.ErrorManager.report (ErrorManager.kt:25) [kcomp]

		appendLine("internal error occurred while compiling (on thread \"${Thread.currentThread().name}\"):")
		appendLine("[*] -- ${this@formatPretty::class.qualifiedName}: ${this@formatPretty.message ?: "no message"}")
		stackTrace.forEach {
			appendLine(it.format())
		}
	}
}