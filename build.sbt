
val commenSettings = Seq(
  organization := "org.datacrafts",
  version := "0.0.1",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test
  )
)

lazy val logging = project.settings(
  commenSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
    "org.slf4j" % "slf4j-log4j12" % "1.7.25" % Test
  )
)