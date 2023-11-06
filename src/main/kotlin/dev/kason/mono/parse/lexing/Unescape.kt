package dev.kason.mono.parse.lexing

// escaping for debug / error
internal fun escapeString(text: String): String =
	text.replace("\\", "\\\\")
		.replace("\n", "\\n")
		.replace("\r", "\\r")
		.replace("\t", "\\t")
		.replace("\"", "\\\"")
		.replace("\'", "\\\'")

fun String.escape(): String = escapeString(this)

