
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

  sonatypeProjectHosting := Some(GitHubHosting("Yongjia Wang", "datacrafts", ""))

)

lazy val logging = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
)

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
  noschema,
  `noschema-thrift` % "test->test"
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
    `noschema-thrift`,
    `noschema-avro`
  )
