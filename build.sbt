ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.0"

val scalaTestVersion = "3.2.16"
val scalaMockitoTestVersion = "3.2.12.0-RC2"
val typeSafeConfigVersion = "1.4.2"
val logbackVersion = "1.2.10"
val sfl4sVersion = "2.0.0-alpha5"
val catsVersion = "2.9.0"
val apacheCommonsVersion = "2.13.0"
val jplVersion = "7.4.0"
val parserCombinatorsVersion = "2.3.0"
val scalaParCollVersion = "1.0.4"
val scalaCheckVersion = "1.17.0"
val regExGeneratorVersion = "1.1.0"
val scalaToolkitVersion = "0.2.0"

lazy val commonDependencies = Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % scalaParCollVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatestplus" %% "mockito-4-2" % scalaMockitoTestVersion % Test,
  "org.scalatest" %% "scalatest-flatspec" % scalaTestVersion % Test,
  "com.typesafe" % "config" % typeSafeConfigVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "jpl" % "jpl" % jplVersion,
  "org.scala-lang" %% "toolkit" % scalaToolkitVersion,
  "org.scala-lang" %% "toolkit-test" % scalaToolkitVersion % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "gemceas",
    libraryDependencies ++= commonDependencies
  ).aggregate(BnfLangModelGenerator,GenericSimUtilities).dependsOn(BnfLangModelGenerator)

lazy val BnfLangModelGenerator = (project in file("BnfLangModelGenerator"))
  .settings(
    name := "BnfLangModelGenerator",
    libraryDependencies ++= commonDependencies ++ Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "commons-io" % "commons-io" % apacheCommonsVersion,
      "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion,
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion,
      "io.github.wolfendale" %% "scalacheck-gen-regexp" % regExGeneratorVersion
    )
  ).dependsOn(GenericSimUtilities)

lazy val GenericSimUtilities = (project in file("GenericSimUtilities"))
  .settings(
    name := "GenericSimUtilities",
    libraryDependencies ++= commonDependencies
  )

scalacOptions ++= Seq(
  "-explain-types", // explain type errors in more detail
  "-feature", // emit warning and location for usages of features that should be imported explicitly
  "-verbose", // Output messages about what the compiler is doing.
  "-Vprofile",
  "-Xfatal-warnings",
  "-version",
  "-encoding", "utf8",
  "-Ysafe-init" //Ensure safe initialization of objects.
)

Global / scalacOptions ++= Seq("-unchecked", "-deprecation", "-color:always")
Global / excludeLintKeys += test / fork

compileOrder := CompileOrder.JavaThenScala
test / fork := true
run / fork := true
run / javaOptions ++= Seq(
  "-Xms1512M",
  "-Xmx20000M"
)

val jarName = "gemceas.jar"
assembly / assemblyJarName := jarName


//Merging strategies
ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case _ => MergeStrategy.first
}

BnfLangModelGenerator / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % Test
)
BnfLangModelGenerator / libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % Test
)
