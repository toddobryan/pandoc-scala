import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "pandoc-scala"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

seq(jacoco.settings : _*)
