import org.jpl7.{Atom, JPL, Query, Term}

import scala.util.Random

object Driver {
  def main(args: Array[String]): Unit = {
    JPL.setTraditional()

    val query: Query = new Query("consult", Array[Term](new Atom("src/main/prolog/scala-test.pl")))
    println("consult " + (if query.hasSolution then "succeeded" else "failed"))
    
    val generateInts = new Query("generate_integers")
    if generateInts.hasSolution then
      val intExprs: List[ArithExp[Int]] =
        ArithmeticExpressionGenerator.generateIntegerExpressions(Random.between(1, 10), List.empty)
      printExpressions(intExprs)

    else
      println("nope")
  }

  private def printExpressions[T](exprList: List[ArithExp[T]]): Unit =
    for
      i <- 0 until exprList.length-1
    do
      print(exprList(i))
      val op = Ops.fromOrdinal(Random.nextInt(Ops.values.length)).value
      print(op)

    println(exprList.last)
}
