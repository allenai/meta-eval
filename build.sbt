import com.typesafe.sbt.SbtNativePackager.packageArchetype

import org.allenai.plugins.CoreDependencies

import org.allenai.plugins.archetypes.LibraryPlugin

import org.allenai.plugins.CoreRepositories.PublishTo

name := "meta-eval"

organization := "org.allenai.scholar.metrics.metadata"

packageArchetype.java_application

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  CoreDependencies.allenAiCommon,
  CoreDependencies.allenAiTestkit % "test",
  "org.apache.commons" % "commons-lang3" % "3.0")

libraryDependencies += "org.jsoup" % "jsoup" % "1.8.1"

dependencyOverrides ++= Set(
  "com.typesafe" % "config" % "1.2.1"
)

PublishTo.ai2Public

enablePlugins(LibraryPlugin)