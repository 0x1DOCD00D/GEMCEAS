/*
 Copyright (c) 8/6/23, 12:21 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
  
 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
  
 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Playground

import scala.annotation.tailrec
import scala.util.Random

object ArithmeticExpressionGenerator {

  private val RAND: Random = new Random()

  // TODO: Look into this to avoid copy/pasting code for other data types
  // https://www.baeldung.com/scala/magnet-pattern
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
