package dev.kason.mono.test

import dev.kason.mono.core.FileSource
import dev.kason.mono.parse.lexing.Lexer
import dev.kason.mono.parse.lexing.TokenStream
import dev.kason.mono.parse.lexing.removeUnnecessaryTokens
import dev.kason.mono.parse.syntax.*

fun main() {
	val postLexerTokenStream =
		TokenStream(Lexer(FileSource("src\\test\\kotlin\\test.mono").document))
			.postOperations()
			.removeUnnecessaryTokens()
	val parser = SyntaxParser(
		prefixRegistry = DynamicParseletRegistry { registerPrefixParselets() },
		infixRegistry = DynamicParseletRegistry { registerInfixParselets() } ,
		typePrefixRegistry = DynamicParseletRegistry { registerTypePrefixParselets() },
		typeInfixRegistry = DynamicParseletRegistry { registerTypeInfixParselets() },
		tokenStream = postLexerTokenStream
	)
	while (parser.cursor.hasNext()) {
		val expression = parser.parseStruct()
		printTree(expression)
	}
}