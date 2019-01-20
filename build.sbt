
import ReleaseTransformations._
import xerial.sbt.Sonatype._

sonatypeProfileName := "org.datacrafts"

val scalaTestVersion = "3.0.5"
val scroogeVersion = "18.7.0"
val shapelessVersion = "2.3.3"
val thriftVersion = "0.11.0"
val avroVersion = "1.8.2"

val commenSettings = Seq(
  organization := "org.datacrafts",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  ),

  releaseCrossBuild := true,

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    // For non cross-build projects, use releaseStepCommand("publishSigned")
    releaseStepCommandAndRemaining("+publishSigned"),
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges
  ),

  publishTo := sonatypePublishTo.value,

  publishMavenStyle := true,

  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),

  sonatypeProjectHosting := Some(GitHubHosting("Yongjia Wang", "datacrafts", "")),

  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-unused"
  )

)

lazy val logging = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
)

lazy val util = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(logging)

lazy val noschema = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % shapelessVersion,
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(logging)

lazy val `noschema-thrift` = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.stripe" %% "scrooge-shapes" % "0.1.0",
    "org.apache.thrift" % "libthrift" % thriftVersion,
    "com.twitter" %% "scrooge-core" % scroogeVersion exclude("com.twitter", "libthrift"),
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(noschema)

lazy val `noschema-avro` = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "org.apache.avro" % "avro" % avroVersion,
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(
  noschema % "test->test;compile->compile",
  `noschema-thrift` % "test->test"
)

lazy val `noschema-json` = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.6",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(
  noschema
)

import sbt.Keys._
val seleniumVersion = "3.141.59"
lazy val dwfpp = project.settings(
  commenSettings,
  mainClass in assembly := Some("org.datacrafts.app.dwfpp.MainApp"),
  libraryDependencies ++= Seq(
    "org.seleniumhq.selenium" % "selenium-chrome-driver" % seleniumVersion,
    "org.seleniumhq.selenium" % "selenium-support" % seleniumVersion,
    "com.typesafe" % "config" % "1.3.3",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25"
  ),
  artifact in (Compile, assembly) := {
    val art = (artifact in (Compile, assembly)).value
    art.withClassifier(Some("assembly"))
  },
  addArtifact(artifact in (Compile, assembly), assembly)
).dependsOn(
  `noschema-json`,
  util
)

lazy val datacrafts = (project in file("."))
  .settings(
    commenSettings,
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )
  .aggregate(
    logging,
    noschema,
    `noschema-avro`,
    `noschema-json`,
    `noschema-thrift`
  )
