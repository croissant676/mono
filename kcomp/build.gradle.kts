
plugins {
    kotlin("jvm") version "1.8.10"
    kotlin("plugin.serialization") version "1.8.10"

}

group = "dev.kason.mono"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.10")
    // serialization
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.5.0")
    // cli
    implementation("org.jetbrains.kotlinx:kotlinx-cli:0.3.5")
    // kotlin logging
    implementation("io.github.microutils:kotlin-logging:2.0.11")
    // logback
    implementation("ch.qos.logback:logback-classic:1.2.3")
    // kotest
    testImplementation("io.kotest:kotest-runner-junit5:4.6.3")
    testImplementation("io.kotest:kotest-assertions-core:4.6.3")

}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}