val dottyVersion = "0.23.0-bin-SNAPSHOT"
  //"0.22.0-bin-20200129-c1612fa-NIGHTLY" //"0.22.0-bin-20200125-c8371e4-NIGHTLY"  // Has new fixes we need

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty_reflection",
    version := "0.0.1",

    scalaVersion := dottyVersion,

    scalacOptions ++= Seq("-language:implicitConversions"), //,"-Xprint:typer"),

    libraryDependencies ++= 
      Seq("ch.epfl.lamp" %% "dotty-compiler" % dottyVersion,
        "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion,
        "ch.epfl.lamp" %% "tasty-core" % dottyVersion,
        "com.novocode" % "junit-interface" % "0.11" % "test")
  )
