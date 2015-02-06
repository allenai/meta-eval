import com.typesafe.sbt.SbtNativePackager.packageArchetype

name := "meta-eval"

version := "1.0"

packageArchetype.java_application

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.11.5",
  "org.apache.commons" % "commons-lang3" % "3.0",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.3",
  "com.typesafe" % "config" % "1.2.1",
  "commons-lang" % "commons-lang" % "2.6",
  "org.slf4j" % "jcl-over-slf4j" % "1.7.7")

libraryDependencies += "io.spray" % "spray-json_2.11" % "1.3.1"



