
import ReleaseTransformations._
import xerial.sbt.Sonatype._

sonatypeProfileName := "org.datacrafts"

val commenSettings = Seq(
  organization := "org.datacrafts",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test
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
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.stripe" %% "scrooge-shapes" % "0.1.0",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(logging)

lazy val datacrafts = (project in file("."))
  .settings(
    commenSettings,
    publish := {},
    publishLocal := {},
    publishArtifact := false
  )
  .aggregate(
    logging,
    noschema
  )
