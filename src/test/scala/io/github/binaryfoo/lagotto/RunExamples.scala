package io.github.binaryfoo.lagotto

import java.io.{File, ByteArrayOutputStream, PrintWriter, FileWriter}
import java.nio.file.{StandardCopyOption, Files}

import io.github.binaryfoo.lagotto.shell.Main
import org.asciidoctor.{SafeMode, OptionsBuilder, Options, AsciiDocDirectoryWalker}
import org.asciidoctor.internal.JRubyAsciidoctor

import scala.io.Source

object RunExamples {

  val Example = "    LAGO: (.*)".r

  def main(args: Array[String]) {
    val outputFile = new File("docs/examples.adoc")
    val out = new PrintWriter(new FileWriter(outputFile))
    Source.fromFile("src/docs/examples.adoc").getLines().foreach {
      case Example(arguments) =>
        out.println("    lago " + arguments + "\n")
        out.println(indent(lagoOutputFrom(cleanAndSplit(arguments))))
      case line =>
        out.println(line)
    }
    out.close()
    move("rtts.csv")
    move("rtts.gp")
    JRubyAsciidoctor.create().renderFile(outputFile, OptionsBuilder.options().safe(SafeMode.UNSAFE))
  }

  def move(file: String): Unit = {
    Files.move(new File(file).toPath, new File(s"docs/$file").toPath, StandardCopyOption.REPLACE_EXISTING)
  }

  def cleanAndSplit(arguments: String): Array[String] = {
    arguments.replaceAll("""'(\S+)'""", "$1").split(" ")
  }

  def lagoOutputFrom(args: Array[String]): String = {
    println(s"Running ${args.mkString(" ")}")
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Main.main(args.toArray)
    }
    out.toString
  }

  def indent(s: String): String = {
    Source.fromString(s).getLines().map("    " + _).mkString("\n")
  }

}
