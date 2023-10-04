
// native packager and scoverage are conflicting...
libraryDependencySchemes ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.5")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.3.9")
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.32")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")
addSbtPlugin("org.typelevel" % "sbt-typelevel-sonatype-ci-release" % "0.5.0-M6")