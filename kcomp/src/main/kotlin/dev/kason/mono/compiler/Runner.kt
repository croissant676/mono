package dev.kason.mono.compiler

import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.error.AnnotatedCodeBlock
import dev.kason.mono.compiler.error.CodeAnnotation
import dev.kason.mono.compiler.error.ErrorManager
import dev.kason.mono.compiler.lexer.Lexer
import dev.kason.mono.compiler.lexer.readAllTokens
import dev.kason.mono.compiler.module.CodeModule

fun main() {
	val source = CodeSource(
		CodeModule(), "test", """
		
		fn main():
			print("hello world")
			'

    """.trimIndent()
	)
	ErrorManager.executeSafe {
		val lexer = Lexer(source)
		lexer.readAllTokens().forEach { println(it) }
	}
}