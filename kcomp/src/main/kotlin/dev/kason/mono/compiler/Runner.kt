package dev.kason.mono.compiler

import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.error.AnnotatedCodeBlock
import dev.kason.mono.compiler.error.CodeAnnotation
import dev.kason.mono.compiler.lexer.Lexer
import dev.kason.mono.compiler.lexer.readAllTokens
import dev.kason.mono.compiler.module.CodeModule

fun main() {
	val source = CodeSource(
		CodeModule(), "test", """
			
		fn main():
			println("Hello, World!")
			let x = () -> Int: 5
			
			let greaterThan2 = [1, 2, 3, 4, 5]
				.filter(: it > 2)
				.map(: it * 2)
				.forEach(b: println(b))
				
			[1, 2, 3, 4]
				.forEach: println(it)
				.map: it * 2
				.forEach: println(it)

			println(x(5))
			
    """.trimIndent()
	)
//	val lexer = Lexer(source)
//	lexer.readAllTokens().forEach { println(it) }
	val block = AnnotatedCodeBlock(source, 1, 10)
	block.annotations += CodeAnnotation(
		source.range(3, 1, 3, 2),
		"expected 'fn' keyword"
	)
	println(block)
}