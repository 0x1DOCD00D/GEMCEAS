import org.jpl7.{Atom, Query, Term}

object Driver {
  def main(args: Array[String]): Unit = {
    // println(Query.hasSolution("['src/main/prolog/scala-test.pl']"))
    val query: Query = new Query("consult", Array[Term](new Atom("src/main/prolog/scala-test.pl")))
    println("consult " + (if query.hasSolution then "succeeded" else "failed"))
  }
}
