lazy val scikoro = crossProject.in(file("scikoro"))
  .settings(
    name := "scikoro",
    organization := "com.github.lettenj61",
    scalaVersion := "2.12.2",
    version      := "0.1.0-SNAPSHOT",
    crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2"),
    scalacOptions in (Compile, compile) ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused-import",
      "-Ywarn-unused"
    ),
    description := "Scala DSL to emurate dice roll",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "utest" % "0.4.7" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val scikoroJVM = scikoro.jvm
lazy val scikoroJS = scikoro.js
