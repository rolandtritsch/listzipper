import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import scoverage.ScoverageKeys._

inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/listzipper")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@blocke.co",
      url("http://www.blocke.co")
    )
  ),
  coverageMinimumStmtTotal    := 92,
  coverageFailOnMinimum       := true,
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignArguments, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true),
))

name := "listzipper"
ThisBuild / scalaVersion     := "3.3.0"
ThisBuild / organization     := "co.blocke"

val scalatest = "org.scalatest" %% "scalatest" % "3.2.17" % "test"
lazy val crossVersions = crossScalaVersions := Seq("2.13.0", "3.3.0")

lazy val root = project
  .in(file("."))
  .settings(settings ++ crossVersions: _*)
  .settings(
    name := "listzipper",
    libraryDependencies ++= Seq(scalatest),
  )

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "8"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "windows-latest")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "CI_SNAPSHOT_RELEASE" -> "+publishSigned"
    )
  )
)

//==========================
// Settings
//==========================
lazy val settings = Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= compilerOptions,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)