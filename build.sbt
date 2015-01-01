name := "lagotto"

organization := "io.github.binaryfoo"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.11.4", "2.10.4")

libraryDependencies += "joda-time" % "joda-time" % "2.6" % "compile"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.hdrhistogram" % "HdrHistogram" % "2.1.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

mainClass in assembly := Some("io.github.binaryfoo.lagotto.shell.Main")

initialCommands in console :=
  """
    |import io.github.binaryfoo.lagotto._
  """.stripMargin
