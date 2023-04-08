import scala.annotation.tailrec
import scala.util.Random

object ArithmeticExpressionGenerator {

  private val RAND: Random = new Random()

  def generateIntegerExpressions(max: Int): List[ArithExp[Int]] = {
    @tailrec
    def helper(max: Int, exprList: List[ArithExp[Int]]): List[ArithExp[Int]] = {
      if max > 0 then
        val exp1 = RAND.nextInt(Int.MaxValue)
        val exp2 = RAND.nextInt(Int.MaxValue)
        val op = Ops.fromOrdinal(RAND.nextInt(Ops.values.length))
        helper(max - 1, exprList :+ new ArithExp[Int](exp1, op, exp2))
      else
        exprList
    }
    
    helper(max, List.empty)
  }
}
