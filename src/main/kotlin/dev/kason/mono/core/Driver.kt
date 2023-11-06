package dev.kason.mono.core

import com.github.ajalt.mordant.terminal.Terminal
import io.github.z4kn4fein.semver.toVersion

object Mono {
	val version = "0.0.1-SNAPSHOT".toVersion()
	val terminal = Terminal()
}
