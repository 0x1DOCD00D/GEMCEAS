/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import java.util.UUID

trait BnFGrammarIR:
  val uuid: UUID = UUID.randomUUID()

trait BnFGrammarIRContainer extends BnFGrammarIR:
  val bnfObjects: List[BnFGrammarIR]
end BnFGrammarIRContainer

type BnFGrammarIrMap = Map[String, List[BnFGrammarIR]]

enum LiteralType:
  case TERM, NONTERM, NTREGEX, REGEXTERM

case class ProductionRule(lhs: BnFGrammarIR, rhs: BnFGrammarIR) extends BnFGrammarIR
case class OptionalConstruct(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer
case class RepeatConstruct(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer
case class GroupConstruct(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer
case class SeqConstruct(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer
case class UnionConstruct(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer
trait IrLiteral extends BnFGrammarIR
case class BnfLiteral(token: String, literalType: LiteralType) extends IrLiteral
case class ProgramEntity(code: String) extends IrLiteral
case class IrError(err: String) extends BnFGrammarIR
