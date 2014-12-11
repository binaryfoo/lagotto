name := "jpos-tools-scala"

organization := "io.github.binaryfoo"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies += "joda-time" % "joda-time" % "2.6" % "compile"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

mainClass in assembly := Some("io.github.binaryfoo.isotools.shell.Main")

initialCommands in console :=
  """
    |import io.github.binaryfoo.isotools._
  """.stripMargin
