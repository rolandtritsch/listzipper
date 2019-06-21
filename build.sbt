//import sbt._
//import sbt.Keys._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.0.1"
ThisBuild / organization     := "co.blocke"

val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % "test"
lazy val crossVersions = crossScalaVersions := Seq("2.12.8","2.13.0")

val basicSettings = Seq(
  coverageMinimum             := 92,  // really this should be 96% but mongo isn't quite up to that yet
  coverageFailOnMinimum       := true,
  parallelExecution := false,
  scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
  Test / parallelExecution := false,
  resolvers += "Bintray Releases" at "http://dl.bintray.com/blocke/releases/",
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignArguments, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true),
)

val pubSettings = Seq (
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "json", "scalajack")
)

lazy val root = (project in file("."))
  .settings(basicSettings ++ crossVersions: _*)
  .settings(pubSettings: _*)
  .settings(
    name := "listzipper",
    libraryDependencies ++= Seq(scalatest)
  )
