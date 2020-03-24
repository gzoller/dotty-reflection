val dottyVersion =  "0.24.0-bin-20200320-30f8c6f-NIGHTLY" 

val pubSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "dotty", "reflection")
)

resolvers += "co.blocke ivy resolver" at "https://dl.bintray.com/blocke/provisional"

lazy val root = project
  .in(file("."))
  .settings(pubSettings: _*)
  .settings(
    name := "dotty_reflection",

    organization := "co.blocke",
    
    resolvers += Resolver.jcenterRepo,

    scalaVersion := dottyVersion,

    Test / parallelExecution := false,

    // scalacOptions ++= Seq("-language:implicitConversions","-Xprint:typer"),
    scalacOptions ++= Seq("-language:implicitConversions"),

    testFrameworks += new TestFramework("munit.Framework"),

    libraryDependencies ++= 
      Seq("ch.epfl.lamp" %% "dotty-compiler" % dottyVersion,
      "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion,
      "ch.epfl.lamp" %% "tasty-core" % dottyVersion,
      "munit" %% "munit" % "0.6.z-3" % "test")  // special build of munit compatible with Dotty 0.24
  )
