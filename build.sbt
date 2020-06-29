name := "dotty-reflection"
organization in ThisBuild := "co.blocke"
val dottyVersion =  "0.25.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(publishArtifact := false)
  .settings(publish := {})
  .aggregate(plugin)

lazy val plugin = project
  .in(file("plugin"))
  .settings(settings)
  .settings(
    name := "reflection_plugin",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    libraryDependencies ++= commonDependencies
  )

//==========================
// Dependencies
//==========================
lazy val dependencies =
  new {
    val dottyCompiler = "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
  }

lazy val commonDependencies = Seq(
  dependencies.dottyCompiler
)

//==========================
// Settings
//==========================
lazy val settings = 
  commonSettings ++
  publishSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers += Resolver.jcenterRepo,
  scalaVersion := dottyVersion
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "dotty", "reflection")
)

/*

val basicSettings = Seq(
  organization := "co.blocke",
  name := "dotty-reflection",
  startYear := Some(2020),
  publishArtifact in (Compile, packageDoc) := false, // disable scaladoc due to bug handling annotations
  scalaVersion := dottyVersion,
  resolvers += Resolver.jcenterRepo,  //<-- Use this one once we're GA and co-publishing to JCenter!
  // coverageMinimum := 98, 
  // coverageFailOnMinimum := true,
  Test / parallelExecution in ThisBuild := false,
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-encoding",
    "UTF8",
    "-unchecked"
  ),
  scalacOptions ++= Seq("-language:implicitConversions")
  // testFrameworks += new TestFramework("munit.Framework"),
  // testOptions in Test += Tests.Argument("-oDF")
)

*/