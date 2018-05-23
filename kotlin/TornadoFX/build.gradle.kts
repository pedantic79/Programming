import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import org.gradle.jvm.tasks.Jar

//group = "com.github.pedantic"
//version = "1.0-SNAPSHOT"

buildscript {
    extra["kotlinVersion"] = "1.2.31"
    extra["junitPlatformVersion"] = "1.0.2"
    val kotlinVersion: String by extra
    val junitPlatformVersion: String by extra

    repositories {
        mavenCentral()
    }

    dependencies {
        classpath(kotlin("gradle-plugin", kotlinVersion))
        classpath("org.junit.platform:junit-platform-gradle-plugin:$junitPlatformVersion")
    }
}

plugins {
    java
}

apply {
    plugin("kotlin")
    plugin("org.junit.platform.gradle.plugin")
}

val kotlinVersion: String by extra
val junitPlatformVersion: String by extra


repositories {
    mavenCentral()
}

dependencies {
    compile(kotlin("stdlib-jdk8", kotlinVersion))
    compile("no.tornado:tornadofx:1.7.13")
    compile("io.reactivex.rxjava2:rxkotlin:2.2.0")
    compile("io.reactivex.rxjava2:rxjava:2.1.7")
    compile("com.github.thomasnield:rxkotlinfx:2.2.2")

    testCompile("org.junit.jupiter:junit-jupiter-api:5.0.2")
    testRuntime("org.junit.platform:junit-platform-launcher:$junitPlatformVersion")
    testRuntime("org.junit.jupiter:junit-jupiter-engine:5.0.2")
    testCompile(group="org.testfx", name="testfx-junit5", version="4.0.13-alpha")
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}

tasks.withType<Jar> {
    manifest {
        attributes["Main-Class"] = "com.github.pedantic.app.MyApp"
        attributes["Class-Path"] = configurations.compile.joinToString(separator = " ") { it.name }
    }
    from(configurations.compile.map { if (it.isDirectory) it else zipTree(it) as Any }) {
        exclude("META-INF/MANIFEST.MF")
        exclude("META-INF/*.SF")
        exclude("META-INF/*.RSA")
    }
}
val compileKotlin: KotlinCompile by tasks
compileKotlin.kotlinOptions {
    jvmTarget = "1.8"
}
val compileTestKotlin: KotlinCompile by tasks
compileTestKotlin.kotlinOptions {
    jvmTarget = "1.8"
}