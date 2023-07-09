/*******************************************************************************
 *
 *  * Copyright (c) 7/9/23, 12:24 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *  *
 *  * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

import scala.util.parsing.combinator._

/*
 * expression ::= sum
 * sum ::= product ~ ("+" ~ product)*
 * product ::= term ~ ("*" ~ term)*
 * term ::= number | "(" ~ expression ~ ")"
 * number ::= """(\+|\-)?[0-9]+(\.[0-9]+)?""".r
 */
object ArithmeticExpressionParser extends RegexParsers:
  import ArithmeticExpression4Parser.*

  def ExpressionNT: Parser[ArithmeticExpression4Parser] = SumNT

  def SumNT: Parser[ArithmeticExpression4Parser] = ProductNT ~ rep("+" ~> ProductNT) ^^ {
    case p ~ Nil => p
    case p ~ prods => Sum(p, prods)
  }

  def ProductNT: Parser[ArithmeticExpression4Parser] = TermNT ~ rep("*" ~> TermNT) ^^ {
    case t ~ Nil => t
    case t ~ terms => Product(t, terms)
  }

  def TermNT: Parser[ArithmeticExpression4Parser] = NumberTerminal | "(" ~> ExpressionNT <~ ")"

  def NumberTerminal: Parser[Number] =
    """([+\-])?[0-9]+(\.[0-9]+)?""".r ^^ (num => Number(num.toDouble))
