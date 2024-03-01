import Playground.{ArithExp, ArithmeticExpressionGenerator, Ops}
import Utilz.CreateLogger
import org.jpl7.{Atom, JPL, Query, Term}

import java.io.{File, PrintWriter}
import scala.util.Random
object Driver {
  def main(args: Array[String]): Unit = {
    val logger = CreateLogger(this.getClass)
    JPL.setTraditional()

    logger.info("running query...")
    val query: Query = new Query("consult", Array[Term](new Atom("src/main/resources/prolog/scala-test.pl")))
    logger.info("consult " + (if query.hasSolution then "succeeded" else "failed"))

    val generateInts = new Query("generate_integers")
    if generateInts.hasSolution then
      val intExprs: List[ArithExp[Int]] =
        ArithmeticExpressionGenerator.generateIntegerExpressions(Random.between(1, 10))
      printExpressions(intExprs)
    else
      logger.error("nope")
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
