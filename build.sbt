val dottyVersion = "0.22.0-bin-SNAPSHOT"
  //"0.22.0-bin-20200129-c1612fa-NIGHTLY" //"0.22.0-bin-20200125-c8371e4-NIGHTLY"  // Has new fixes we need

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty_reflection",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    scalacOptions ++= Seq("-language:implicitConversions"), //,"-Xprint:typer"),

    // unmanagedBase := ("Users"/"wmy965"/"git"/"dotty"/"compiler"/"target"/"scala-0.22"/"classes"),

    // libraryDependencies += "ch.epfl.lamp" %% "dotty-library" % "0.22.0-bin-SNAPSHOT-nonbootstrapped",
    libraryDependencies += "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion,
    libraryDependencies += "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion,    
    libraryDependencies += "ch.epfl.lamp" %% "tasty-core" % dottyVersion
    // libraryDependencies += "ch.epfl.lamp" %% "dotty-staging" % dottyVersion
  )
