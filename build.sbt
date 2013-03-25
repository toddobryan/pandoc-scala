// import de.johoop.jacoco4sbt._
// import JacocoPlugin._

name := "pandoc-scala"

version := "0.1"

scalaVersion := "2.10.1"

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.10.1",
  "org.scala-lang" % "scala-reflect" % "2.10.1",
  "org.scala-lang" % "scala-swing" % "2.10.1",
  "org.scala-lang" % "scala-actors" % "2.10.1",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "com.chuusai" %% "shapeless" % "1.2.4",
  "org.scalaz" %% "scalaz-core" % "6.0.4"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// seq(jacoco.settings : _*)
