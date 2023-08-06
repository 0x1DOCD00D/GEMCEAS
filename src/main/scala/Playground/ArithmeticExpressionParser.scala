/*
 Copyright (c) 8/6/23, 12:21 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
  
 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
  
 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Playground

/*******************************************************************************
 *
 *  * Copyright (c) 7/9/23, 12:24 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *  *
 *  * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

import scala.util.parsing.combinator.*

/*
 Using a standard grammar for arithmetic expressions
 expression ::= sum
 sum ::= product {"+" product}
 product ::= ["+"|"-"] term {"*" term}
 term ::= number | "(" expression ")"
 number ::= """(\+|\-)?[0-9]+(\.[0-9]+)?""".r
 */
object ArithmeticExpressionParser extends JavaTokenParsers:
  import ArithmeticExpression4Parser.*

  def ExpressionNT: Parser[ArithmeticExpression4Parser] = SumNT

  private def checkSign(sign: Option[String]): ArithmeticExpression4Parser =
    sign match
      case Some(s) => if s == "+" then PLUS else if s == "-" then MINUS else ERROR
      case None => ERROR

  def SumNT: Parser[ArithmeticExpression4Parser] = ProductNT ~ rep("+" ~> ProductNT) ^^ {
    case p ~ Nil => p
    case p ~ products => Sum(p, products)
  }

  def ProductNT: Parser[ArithmeticExpression4Parser] = opt("-"|"+") ~ TermNT ~ rep("*" ~> TermNT) ^^ {
    case sign ~ t ~ Nil => if sign.nonEmpty then Sign(checkSign(sign), t) else t
    case sign ~ t ~ terms => if sign.nonEmpty then Sign(checkSign(sign), Product(t, terms)) else Product(t, terms)
  }

  def TermNT: Parser[ArithmeticExpression4Parser] = NumberTerminal | "(" ~> ExpressionNT <~ ")"

  def NumberTerminal: Parser[Literal] =
    """([+\-])?[0-9]+(\.[0-9]+)?""".r ^^ { input => Literal(input.toDouble) }

  @main def runMain_ArithmeticExpressionParser(): Unit =
    parseAll(ArithmeticExpressionParser.ExpressionNT, "- (3   * (-2+-1))") match
      case failed: ArithmeticExpressionParser.Failure => println(failed.msg)
      case success => println(success)
