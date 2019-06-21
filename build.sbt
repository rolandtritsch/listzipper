import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "co.blocke"

val scalatest = "org.scalatest" %% "scalatest" % "3.0.8" % "test"

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

lazy val root = (project in file("."))
  .settings(basicSettings: _*)
  .settings(
    name := "listzipper",
    libraryDependencies ++= Seq(scalatest)
  )
