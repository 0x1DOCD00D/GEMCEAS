import Playground.{ArithExp, ArithmeticExpressionGenerator, Ops}
import Utilz.CreateLogger
import org.jpl7.{Atom, JPL, Query, Term}

import java.io.{File, PrintWriter}
import scala.util.Random

/*
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.2"

val scalaTestVersion = "3.2.17"
val scalaMockitoTestVersion = "3.2.12.0-RC2"
val typeSafeConfigVersion = "1.4.3"
val logbackVersion = "1.4.14"
val sfl4sVersion = "2.0.0-alpha5"
val catsVersion = "2.10.0"
val apacheCommonsVersion = "2.15.1"
val parserCombinatorsVersion = "2.3.0"
val scalaParCollVersion = "1.0.4"

lazy val commonDependencies = Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % scalaParCollVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatestplus" %% "mockito-4-2" % scalaMockitoTestVersion % Test,
  "com.typesafe" % "config" % typeSafeConfigVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion
)

lazy val root = (project in file("."))
  .settings(
    name := "gemceas",
    libraryDependencies ++= commonDependencies
  ).aggregate(BnfLangModelGenerator,GenericSimUtilities)
  .dependsOn(Jpl)
  .dependsOn(BnfLangModelGenerator)

lazy val Jpl = RootProject(uri("https://github.com/SWI-Prolog/packages-jpl.git#V9.3.1"))
Jpl / scalaVersion := "3.2.2"
Jpl / javacOptions ++= Seq("-source", "17", "-target", "17")

lazy val BnfLangModelGenerator = (project in file("BnfLangModelGenerator"))
  .settings(
    name := "BnfLangModelGenerator",
    libraryDependencies ++= commonDependencies ++ Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "commons-io" % "commons-io" % apacheCommonsVersion,
      "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion
    )
  ).dependsOn(GenericSimUtilities)

lazy val GenericSimUtilities = (project in file("GenericSimUtilities"))
  .settings(
    name := "GenericSimUtilities",
    libraryDependencies ++= commonDependencies
  )

scalacOptions ++= Seq(
  "-deprecation", // emit warning and location for usages of deprecated APIs
  "-explain-types", // explain type errors in more detail
  "-feature", // emit warning and location for usages of features that should be imported explicitly
  "-verbose", // Output messages about what the compiler is doing.
  "-Vprofile",
  "-Xfatal-warnings",
  "-version",
  "-encoding", "utf8",
  "-Ysafe-init" //Ensure safe initialization of objects.
)

ThisBuild / scalacOptions ++= Seq("-unchecked", "-color:always")

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
 
* */

object Driver {
  def main(args: Array[String]): Unit = {
    val logger = CreateLogger(this.getClass)
    JPL.setTraditional()

    logger.info("running query...")
    val query: Query = new Query("consult", Array[Term](new Atom("/Users/drmark/github/gemceas/src/main/prolog/scala-test.pl")))
    logger.info("consult " + (if query.hasSolution then "succeeded" else "failed"))

//    val writer = createJavaFile()

    val generateInts = new Query("generate_integers")
    if generateInts.hasSolution then
      val intExprs: List[ArithExp[Int]] =
        ArithmeticExpressionGenerator.generateIntegerExpressions(Random.between(1, 10))
      // printExpressions(intExprs)
//      writeDataType(writer, "INT")
//      writeExpressions(writer, intExprs)
//
    else
      logger.error("nope")

//    closeJavaFile(writer)
  }

  private def printExpressions[T](exprList: List[ArithExp[T]]): Unit = {
    for
      i <- 0 until exprList.length - 1
    do
      print(exprList(i))
      val op = Ops.fromOrdinal(Random.nextInt(Ops.values.length)).value
      print(op)

    println(exprList.last)
  }

  private def createJavaFile(): PrintWriter = {
    val writer = new PrintWriter(new File("src/main/java/Test.java"))
    writer.write("class Test {\n")
    writer.write("\tpublic static void main(String[] args) {\n")

    writer
  }

  private def writeDataType(writer: PrintWriter, dataType: String): Unit = {
    val varName = Random.alphanumeric.dropWhile(_.isDigit).take(5).mkString
    dataType match
      case "INT" => writer.write("\t\tint " + varName + " = ")
      case _ => throw new Exception(s"$dataType data type is not supported")
  }

  private def writeExpressions[T](writer: PrintWriter, exprList: List[ArithExp[T]]): Unit = {
    for
      i <- 0 until exprList.length - 1
    do
      writer.write(exprList(i).toString)
      val op = Ops.fromOrdinal(Random.nextInt(Ops.values.length)).value
      writer.write(op)

    writer.write(exprList.last.toString + ";\n")
  }

  private def closeJavaFile(writer: PrintWriter): Unit = {
    writer.write("\t}\n")   // end main method
    writer.write("}\n")   // end class

    writer.close()
  }
}
