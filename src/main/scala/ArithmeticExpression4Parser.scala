/*******************************************************************************
 *
 *  Copyright (c) 7/9/23, 10:47 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

/*
 Using a standard grammar for arithmetic expressions
 expression ::= sum
 sum ::= product {"+" product}
 product ::= term {"*" term}
 term ::= number | "(" expression ")"
 number ::= """(\+|\-)?[0-9]+(\.[0-9]+)?""".r
 */

enum ArithmeticExpression4Parser:
  case Sum(operand1: ArithmeticExpression4Parser, operands: List[ArithmeticExpression4Parser])
  case Product(operand1: ArithmeticExpression4Parser, operands: List[ArithmeticExpression4Parser])
  case Sign(signType: ArithmeticExpression4Parser, exp: ArithmeticExpression4Parser)
  case Literal(value: Double)
  case PLUS, MINUS, ERROR