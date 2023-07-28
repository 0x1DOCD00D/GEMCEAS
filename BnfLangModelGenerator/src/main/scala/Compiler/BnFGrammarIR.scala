/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

trait BnFGrammarIR
type BnFGrammarIrMap = Map[String, List[BnFGrammarIR]]

enum LiteralType:
  case TERM, NONTERM, NTREGEX

case class ProductionRule(lhs: BnFGrammarIR, rhs: BnFGrammarIR) extends BnFGrammarIR
case class OptionalConstruct(bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIR
case class RepeatConstruct(bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIR
case class GroupConstruct(bnfObjects: BnFGrammarIR) extends BnFGrammarIR
case class SeqConstruct(bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIR
case class UnionConstruct(bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIR
trait IrLiteral extends BnFGrammarIR
case class BnfLiteral(token: String, literalType: LiteralType) extends IrLiteral

case class IrError(err: String) extends BnFGrammarIR
