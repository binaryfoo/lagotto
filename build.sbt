name := "lagotto"

organization := "io.github.binaryfoo"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.11.4", "2.10.4")

libraryDependencies += "joda-time" % "joda-time" % "2.6" % "compile"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.hdrhistogram" % "HdrHistogram" % "2.1.2"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.asciidoctor" % "asciidoctorj" % "1.5.2" % "test"

mainClass in assembly := Some("io.github.binaryfoo.lagotto.shell.Main")

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

seq(bintraySettings:_*)

seq(bintrayPublishSettings:_*)

bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("jpos", "log", "grep")

initialCommands in console :=
  """
    |import io.github.binaryfoo.lagotto._
  """.stripMargin
