import Utilz.CreateLogger
import org.jpl7.{Atom, Integer, JPL, Query, Term}

import scala.annotation.tailrec
import scala.util.Random

object Playground {
  def main(args: Array[String]): Unit = {
    val logger = CreateLogger(this.getClass)
    JPL.setTraditional()
    logger.info("Starting JPL run")

    val query: Query = new Query("consult", Array[Term](new Atom("src/main/prolog/scala-test.pl")))
    println("consult " + (if query.hasSolution then "succeeded" else "failed"))

    checkDivPredicate()
    checkMulPredicate()

    rec(predicate = "add")
    rec(predicate = "mul")

  }

  private def checkDivPredicate(): Unit = {
    val query1: Query = new Query("div", Array[Term](new Integer(1), new Integer(2)))
    println("div(1,2) " + (if query1.hasSolution then "succeeded" else "failed"))

    val query2: Query = new Query("div", Array[Term](new Integer(1), new Integer(0)))
    println("div(1,0) " + (if query2.hasSolution then "succeeded" else "failed"))
  }

  private def checkMulPredicate(): Unit = {
    val query1: Query = new Query("mul", Array[Term](new Integer(1), new Integer(-2)))
    println("mul(1,-2) " + (if query1.hasSolution then "succeeded" else "failed"))

    val query2: Query = new Query("mul", Array[Term](new Integer(-1), new Integer(2)))
    println("mul(-1,2) " + (if query2.hasSolution then "succeeded" else "failed"))

    val query3: Query = new Query("mul", Array[Term](new Integer(1), new Integer(2)))
    println("mul(1,2) " + (if query3.hasSolution then "succeeded" else "failed"))
  }

  /**
   * Tail recursive method that generates random values for predicates till it passes
   */
  @tailrec
  private def rec(predicate: String): Unit = {
    val lhs = Random.between(Int.MinValue, Int.MaxValue)
    val rhs = Random.between(Int.MinValue, Int.MaxValue)
    val query: Query = new Query(s"$predicate", Array[Term](new Integer(lhs), new Integer(rhs)))
    if query.hasSolution then
      println(s"$predicate($lhs, $rhs) succeeded")
    else
      rec(predicate)
  }
}
