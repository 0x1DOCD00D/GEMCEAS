/*
 Copyright (c) 8/1/23, 10:42 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, BnfLiteral, GroupConstruct, OptionalConstruct, ProductionRule, ProgramEntity, PrologFact, PrologFactsBuilder, RepeatConstruct, RepeatPrologFact, SeqConstruct, UnionConstruct}
import Randomizer.SupplierOfRandomness
import Utilz.ConfigDb

object OptionalConstructProcessor extends ((OptionalConstruct, Boolean) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: OptionalConstruct, v2: Boolean): List[BnFGrammarIR] =
    if v2 then List()
    else
      if SupplierOfRandomness.`YesOrNo?`()() then v1.bnfObjects
      else List()

object RepeatConstructProcessor extends ((RepeatConstruct, Boolean) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: RepeatConstruct, v2: Boolean): List[BnFGrammarIR] =
    if v1.bnfObjects.isEmpty then
      logger.error(s"Repeat construct ${v1.bnfObjects.mkString(",")} is empty")
      List()
    else if v1.bnfObjects.length ==  1 then
      v1.bnfObjects.head match
        case groupConstruct: GroupConstruct =>
          GroupConstructProcessor(groupConstruct, if v2 then 0 else SupplierOfRandomness.onDemandInt(pmaxv = ConfigDb.`Gemceas.Generator.maxRepeatConstruct`))
        case _ =>
          logger.error(s"Repeat construct ${v1.bnfObjects.mkString(",")} is not a group construct")
          List()
    else
      logger.error(s"Repeat construct ${v1.bnfObjects.mkString(",")} has more than one element")
      List()

object PrologTemplateProcessor extends ((List[BnFGrammarIR], PrologFactsBuilder) => Option[PrologFact]) with DeriveConstructs:
  override def apply(elements: List[BnFGrammarIR], template: PrologFactsBuilder): Option[PrologFact] =
    template.build(elements) match
      case Left(errorMsg) =>
        logger.error(errorMsg)
        None
      case Right(prologFact) => Option(prologFact)

/*
 All rhs elements are organized in groups, implicitly or explicitly. If a prolog template is added as a special
 terminal then its instance should be created and populated with the derived elements of the rhs group.
 First, check4PrologTemplate is called to determine if the group parameter contains a prolog template and if so
 the PrologTemplateProcessor is applied to the elements of the group.
* */
object GroupConstructProcessor extends ((GroupConstruct, Int) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: GroupConstruct, repeat: Int = 1): List[BnFGrammarIR] = {
    check4PrologTemplate(v1) match
      case Some(prologTemplate) =>
        val res = PrologTemplateProcessor(v1.bnfObjects.filterNot(_.isInstanceOf[PrologFactsBuilder]), prologTemplate) match
            case Some(prologFact) => List(prologFact)
            case None => v1.bnfObjects.filterNot(_.isInstanceOf[PrologFactsBuilder]) //failed to process a prolog template but continue the rewriting process
        if repeat <= 1 then res
        else List(RepeatPrologFact(List.fill(repeat)(res).flatten))
      case None =>
        val res = v1.bnfObjects.filterNot(_.isInstanceOf[PrologFactsBuilder]) //more than one prolog template is not allowed
        if repeat <= 1 then res
        else
          List.fill(repeat)(res).flatten
  }
  private def check4PrologTemplate(v1: GroupConstruct): Option[PrologFactsBuilder] =
    val prologTemplateExists: Int = v1.bnfObjects.count(e => e.isInstanceOf[PrologFactsBuilder])
    if prologTemplateExists > 1 then
      logger.error(s"More than one PrologFactsBuilder is present and therefore ignored in the group construct ${v1.bnfObjects.mkString(",")}")
      None
    else if prologTemplateExists == 1 then v1.bnfObjects.find(e => e.isInstanceOf[PrologFactsBuilder]).asInstanceOf[Option[PrologFactsBuilder]]
    else None
  end check4PrologTemplate

end GroupConstructProcessor


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
          case Some(value) => deriveElement(value)
          case None =>
            logger.error(s"Union construct ${v1.bnfObjects.mkString(",")} has no terminating element")
            List()
      else
        val ind = SupplierOfRandomness.onDemandInt(pmaxv = sz)
        v1.bnfObjects.lift(ind) match
          case Some(value) => deriveElement(value)
          case None =>
            logger.error(s"Union construct ${v1.bnfObjects.mkString(",")} has no element with the index $ind")
            List()

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
