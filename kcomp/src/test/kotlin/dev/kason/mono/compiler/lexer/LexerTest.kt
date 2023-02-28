package dev.kason.mono.compiler.lexer

import dev.kason.mono.compiler.util.testSource
import io.kotest.assertions.throwables.shouldThrowAny
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class LexerTest : StringSpec({
    "number literal deci basic" {
        val source = testSource("123")
        val lexer = Lexer(source)
        val token = lexer.readToken()
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
    }
    "number literal float" {
        val source = testSource("123.456")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.FLOATING_POINT
    }
    "number literal hex" {
        val source = testSource("0x123")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.HEXADECIMAL
    }
    "number literal octal" {
        val source = testSource("0o123")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.OCTAL
    }
    "number literal octal without o" {
        val source = testSource("0123")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.OCTAL
    }
    "number literal binary" {
        val source = testSource("0b1010")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.BINARY
    }
    "number literal exponent" {
        val source = testSource("1e10")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.EXPONENT
    }
    "number literal exponent with sign" {
        val source = testSource("1e+10")
        val lexer = Lexer(source)
        val token = lexer.readToken() as NumberLiteralToken
        token.tokenType shouldBe TokenType.NUMBER_LITERAL
        token.type shouldBe NumberLiteralType.EXPONENT
    }

    "string literal" {
        val source = testSource("\"hello world\"")
        val lexer = Lexer(source)
        val token = lexer.readToken() as StringLiteralToken
        token.tokenType shouldBe TokenType.STRING_LITERAL
        token.value shouldBe "\"hello world\""
    }

    "string literal with escape" {
        val source = testSource("\"hello \\\"world\\\"\"")
        val lexer = Lexer(source)
        val token = lexer.readToken() as StringLiteralToken
        token.tokenType shouldBe TokenType.STRING_LITERAL
        token.value shouldBe "\"hello \\\"world\\\"\""
    }
    "singlequote string literal" {
        val source = testSource("'hello world'")
        val lexer = Lexer(source)
        val token = lexer.readToken() as StringLiteralToken
        token.tokenType shouldBe TokenType.STRING_LITERAL
        token.value shouldBe "'hello world'"
        token.singleQuote shouldBe true
    }
    "singlequote string literal should terminate with error on newline" {
        val source = testSource("'hello \n world'")
        val lexer = Lexer(source)
        shouldThrowAny {
            lexer.readToken()
        }
    }
})