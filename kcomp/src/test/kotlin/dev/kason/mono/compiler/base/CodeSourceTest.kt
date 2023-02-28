package dev.kason.mono.compiler.base

import dev.kason.mono.compiler.util.testSource
import io.kotest.assertions.throwables.shouldThrowAny
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CodeSourceTest : StringSpec({
    "index" {
        val source = testSource("hello world")
        val index = source.index(1, 1)
        index.column shouldBe 1
        index.line shouldBe 1
        index.index shouldBe 0
        index.line() shouldBe "hello world"
    }
    "index (newline)" {
        val source = testSource("here's some random text\nwith a new line")
        val index = source.index(2, 1)
        index.column shouldBe 1
        index.line shouldBe 2
        index.index shouldBe 24
        index.line() shouldBe "with a new line"
    }
    "index (boundary)" {
        val source = testSource("here's some random text\nwith a new line")
        val index = source.index(2, 0)
        index.column shouldBe 0
        index.line shouldBe 2
        index.index shouldBe 23
        index.line() shouldBe "with a new line"
    }
    "range" {
        val source = testSource("hello world")
        val range = source.range(0, 5)
        range.start.column shouldBe 1
        range.start.line shouldBe 1
        range.start.index shouldBe 0
        range.start.line() shouldBe "hello world"
        range.end.column shouldBe 6
        range.end.line shouldBe 1
        range.end.index shouldBe 5
        range.end.line() shouldBe "hello world"
        range.content() shouldBe "hello"
    }
    "range (newline)" {
        val source = testSource("here's some random text\nwith a new line")
        val range = source.range(0, 24)
        range.start.column shouldBe 1
        range.start.line shouldBe 1
        range.start.index shouldBe 0
        range.start.line() shouldBe "here's some random text"
        range.end.column shouldBe 1
        range.end.line shouldBe 2
        range.end.index shouldBe 24
        range.end.line() shouldBe "with a new line"
    }
    "range (on boundary)" {
        val source = testSource("here's some random text\nwith a new line")
        val range = source.range(0, 23)
        range.start.column shouldBe 1
        range.start.line shouldBe 1
        range.start.index shouldBe 0
        range.start.line() shouldBe "here's some random text"
        range.end.column shouldBe 0
        range.end.line shouldBe 2
        range.end.index shouldBe 23
        range.end.line() shouldBe "with a new line"
    }
    "more properties" {
        val source = testSource("random text\n\n\nsome more newlines here\n\n")
        source.content.length shouldBe 39
        source.lineCount() shouldBe 6
        source.content shouldBe source.completeRange.content()
    }
    "error" {
        val source = testSource("hello world")
        shouldThrowAny {
            source.line(2)
        }
        shouldThrowAny {
            source.index(1, 13)
        }
        shouldThrowAny {
            source.index(2, 1)
        }
        shouldThrowAny {
            source.index(13).charValue()
        }
    }
})