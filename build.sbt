name := "jpos-tools-scala"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "joda-time" % "joda-time" % "2.6" % "compile"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

initialCommands in console :=
  """
    |import io.github.binaryfoo.isotools._
  """.stripMargin
