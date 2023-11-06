package dev.kason.mono.test

import dev.kason.mono.core.FileSource
import dev.kason.mono.parse.lexing.Lexer
import dev.kason.mono.parse.lexing.TokenStream

fun main() {
	val file = FileSource("C:\\Users\\crois\\IdeaProjects\\a\\mono\\src\\test\\kotlin\\file.mono")
	val lexer = Lexer(file.document)
	val tokenStream = TokenStream(lexer)
	println(tokenStream)
	println(tokenStream.postOperations())
}