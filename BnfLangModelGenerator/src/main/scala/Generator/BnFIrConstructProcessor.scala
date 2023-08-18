/*
 Copyright (c) 8/1/23, 10:42 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, BnfLiteral, GroupConstruct, OptionalConstruct, ProductionRule, ProgramEntity, RepeatConstruct, SeqConstruct, UnionConstruct}
import Randomizer.SupplierOfRandomness

object OptionalConstructProcessor extends ((OptionalConstruct, Boolean) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: OptionalConstruct, v2: Boolean): List[BnFGrammarIR] =
    if v2 then List()
    else
      if SupplierOfRandomness.`YesOrNo?`()() then v1.bnfObjects
      else List()

object RepeatConstructProcessor extends ((RepeatConstruct, Boolean) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: RepeatConstruct, v2: Boolean): List[BnFGrammarIR] =
    val repeats = if v2 then 0 else SupplierOfRandomness.onDemandInt(pmaxv = 10)
    List.fill(repeats)(v1.bnfObjects).flatten

object GroupConstructProcessor extends (GroupConstruct => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: GroupConstruct): List[BnFGrammarIR] = v1.bnfObjects

object SeqConstructProcessor extends (SeqConstruct => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: SeqConstruct): List[BnFGrammarIR] = v1.bnfObjects

object UnionConstructProcessor extends ((UnionConstruct, Boolean) => List[BnFGrammarIR]) with DeriveConstructs:
  def findTermination(e: BnFGrammarIR): Boolean = {
    import Compiler.TerminationData.DIRECTTERMINATION
    ProgramGenerator.lookup(e.uuid) match
      case Some(value) => if value == DIRECTTERMINATION then true else false
      case None => true
  }

  override def apply(v1: UnionConstruct, v2: Boolean): List[BnFGrammarIR] =
    val sz = v1.bnfObjects.length
    if sz < 1 then List()
    else if sz == 1 then v1.bnfObjects
    else
      if v2 then
        v1.bnfObjects.find(e => findTermination(e)) match
          case Some(value) => funcContainedConstructs(value)
          case None => List()
      else
        val ind = SupplierOfRandomness.onDemandInt(pmaxv = sz)
        v1.bnfObjects.lift(ind) match
          case Some(value) => funcContainedConstructs(value)
          case None => List()

object LiteralProcessor extends (BnfLiteral => List[BnFGrammarIR]) with DeriveConstructs:
  import Compiler.LiteralType.*
  import org.scalacheck.*
  import wolfendale.scalacheck.regexp.RegexpGen

  override def apply(v1: BnfLiteral): List[BnFGrammarIR] =
    v1 match
      case nt @ BnfLiteral(token, NONTERM) =>
        ProgramGenerator.expandNT(nt) match
          case Some(r) => List(r.rhs)
          case None => List()
      case BnfLiteral(token, NTREGEX) => List(ProgramEntity(token))
      case BnfLiteral(token, TERM) =>
        List(ProgramEntity(token))
      case BnfLiteral(token, REGEXTERM) =>
        val generator: Gen[String] = RegexpGen.from(token)
        List(ProgramEntity(generator.sample.getOrElse(0).toString))
