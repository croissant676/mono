package dev.kason.mono.compiler.util

import dev.kason.mono.compiler.base.CodeSource
import dev.kason.mono.compiler.module.CodeModule

fun testSource(content: String): CodeSource = CodeSource(CodeModule(), "test", content)