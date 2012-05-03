import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "pandoc-scala"

version := "0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.1" % "test",
  "com.chuusai" %% "shapeless" % "1.2.0"
)

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-Ydependent-method-types"

seq(jacoco.settings : _*)



