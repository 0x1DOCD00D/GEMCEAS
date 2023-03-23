ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "gemceas",
    libraryDependencies += "jpl" % "jpl" % "7.4.0"
  )

// use jpl.jar downloaded with SWI Prolog. Using a different version throws an 
// "Unsupported blob type passed from Prolog" error
// libraryDependencies += "jpl" % "jpl" % "7.4.0"
