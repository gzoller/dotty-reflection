name := "dotty-reflection"
organization in ThisBuild := "co.blocke"
val dottyVersion =  "0.26.0-RC1" //"0.25.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(settings)
  .settings(
    name := "reflection_library",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    doc := null,  // disable dottydoc for now
    sources in (Compile, doc) := Seq(),
    Test / parallelExecution := false,
    libraryDependencies ++= commonDependencies
  )

//==========================
// Dependencies
//==========================
lazy val dependencies =
  new {
    val dottyCompiler = "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
    val dottyInspection = "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion
    val munit = "org.scalameta" %% "munit" % "0.7.11" % Test
  }

lazy val commonDependencies = Seq(
  dependencies.dottyCompiler,
  dependencies.dottyInspection,
  dependencies.munit
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
  scalaVersion := dottyVersion,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "dotty", "reflection")
)