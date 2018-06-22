
val commenSettings = Seq(
  organization := "org.datacrafts",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) {
      Some("snapshots" at nexus + "content/repositories/snapshots/org/datacrafts")
    }
    else {
      Some("releases"  at nexus + "service/local/staging/deploy/maven2/org/datacrafts")
    }

  }
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
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
).dependsOn(logging)

lazy val datacrafts = (project in file("."))
  .settings(
    publish := {}
  )
  .aggregate(
    logging,
    noschema)