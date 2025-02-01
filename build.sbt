// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "1.0.0"
ThisBuild / organization     := "ro.upb.nrs"

val chiselVersion = "3.5.2"

publishTo := Some("Repsy Managed Repository" at "https://repo.repsy.io/mvn/sdcioc/nrs")
credentials += Credentials("Repsy Managed Repository", "repo.repsy.io", "sdcioc", "12#$qwER")
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)

lazy val root = (project in file("."))
  .settings(
    name := "hgl",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0-RC2" % "test",
      "edu.berkeley.cs" %% "dsptools" % "1.5.0-RC2",
      "ro.upb.nrs" %% "sl" % "1.0.0",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    externalResolvers ++= Seq("Repsy Managed Repository" at "https://repo.repsy.io/mvn/sdcioc/nrs"),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )

