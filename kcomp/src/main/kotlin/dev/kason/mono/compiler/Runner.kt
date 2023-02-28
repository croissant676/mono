package dev.kason.mono.compiler

import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.lexer.Lexer
import dev.kason.mono.compiler.lexer.readAllTokens
import dev.kason.mono.compiler.module.CodeModule

fun main() {
	val source = CodeSource(
		CodeModule(), "test", """
		fn main():
			println("Hello, World!")
			let number = 1 + 5
			println(number)
		
    """.trimIndent()
	)
	val lexer = Lexer(source)
	lexer.readAllTokens().forEach { println(it) }
}