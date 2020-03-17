val dottyVersion = "0.23.1-special-SNAPSHOT" //"0.23.0-bin-20200313-ea27651-NIGHTLY"

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
      "org.scalameta" %% "munit" % "0.6.0-M1+3-a4a1d470-SNAPSHOT" % "test")
  )

  /*
  NOTE: At this time, munit doesn't have a build that supports the latest-greatest Dotty release, so I 
  had to build one locally.  It is a little tricky.    
    1.  clone their repo (https://github.com/scalameta/munit)
    2.  update the Dotty version in their build.sbt file 
    3.  (Important!) git add -A
    4.  (Important!) git commit -m "some msg"
    (You do *not* need to push this anywhere... munit's multiple project parts won't have the same version # if you don't do this!)
    5.  sbt + publishLocal
    6.  Get the generated local snapshot version number for munit and paste here in this build.sbt file

  When Dotty 0.23 comes out and munit is likewise updated, this will all become unnecessary.
  */