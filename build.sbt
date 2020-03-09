val dottyVersion = "0.23.0-bin-SNAPSHOT" //"0.23.0-bin-20200220-228e593-NIGHTLY" 

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty_reflection",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    Test / parallelExecution := false,

    // scalacOptions ++= Seq("-language:implicitConversions","-Xprint:typer"),
    scalacOptions ++= Seq("-language:implicitConversions"),

    testFrameworks += new TestFramework("munit.Framework"),

    libraryDependencies ++= 
      Seq("ch.epfl.lamp" %% "dotty-compiler" % dottyVersion,
      "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion,
      "ch.epfl.lamp" %% "tasty-core" % dottyVersion,
      "org.scalameta" %% "munit" % "0.5.2+6-a64ba690-SNAPSHOT" % "test")
  )
