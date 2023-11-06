package dev.kason.mono.test

import java.time.Duration
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.*

fun main() {
	val text = readln().split(" ")
	val dtf = DateTimeFormatter.ofPattern("HH:mm:ss-DD/MM/yyyy")
	val first = LocalDateTime.parse("${text[0]}-${text[2]}", dtf)
	val second = LocalDateTime.parse("${text[1]}-${text[3]}", dtf)
	val s = text[4]
	val index = s.indices.first { !s[it].isDigit() }
	val coeff = s.substring(0, index).toDouble()
	val time = ChronoUnit.valueOf(s.substring(index).uppercase() + "S").duration.multipliedBy(coeff.toLong()).toMillis()
	val res = Duration.between(first, second).seconds * 1000 / time.toDouble()
	System.out.printf("%.2f turns", res)
}