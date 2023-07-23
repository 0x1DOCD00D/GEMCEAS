/*******************************************************************************
 * Copyright (c) 7/17/23, 6:29 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package LexerParser

import scala.util.parsing.input.Positional

sealed trait LexerToken extends Positional
sealed trait Literal extends LexerToken
sealed trait Separators extends LexerToken
sealed trait LogicalOperations extends LexerToken

case class Nonterminal(id: String) extends Literal
case class NonterminalRegex(id: String) extends Literal
case class Terminal(id: String) extends Literal
case class RegexString(id: String) extends Literal

case class ISDEFINEDAS() extends Separators
case class COMMENT() extends Separators
case class UNKNOWNTOKEN() extends Separators
case class SEMICOLON() extends Separators

case class LEFTPAREN() extends LogicalOperations
case class RIGHTPAREN() extends LogicalOperations
case class VERTICALBAR() extends LogicalOperations
case class BRA() extends LogicalOperations
case class KET() extends LogicalOperations
case class CURLYBRA() extends LogicalOperations
case class CURLYKET() extends LogicalOperations

