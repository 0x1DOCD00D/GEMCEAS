/*
 Copyright (c) 8/1/23, 10:42 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, BnfLiteral, GroupConstruct, OptionalConstruct, ProductionRule, ProgramEntity, PrologFactsBuilder, RepeatConstruct, SeqConstruct, UnionConstruct}
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

/*
  The number of args in a template should be equal to the number of top level terminals + nonterminals + repetition/optional constructs (if any). For example, this rule
  product_div ::=
    ["+"|"-"] term {("*"|"/") term}
    "==>> product_div(_, NumberOrExpression, term_repetition(Sign, Term))";
  has three top level constructs,
  the optional construct["+"|"-"],
  a non terminal / term and
  a repetition construct{("*"|"/") term}
  Each of these constructs must correspond to either an _, a variable, or a functor in the prolog template. Functors should be use for repetition/optional constructs as shown in the example above where the repetition construct{("*"|"/") term} corresponds to the functor term_repetition(Sign, Term) functor. The number of params in the functor must match the number of elements in the repetition/optional construct.
  In cases where a union | is present in an repetition/optional construct and the number of elements on either side of the union differ, the number of params in the corresponding functor must be equal to the largest number of elements in all of those options. For example, in
  product_div ::=
    ["+"|("-" nt)] term {("*"|"/") term}
    "==>> product_div(OptCons(Sign, SomeNt), NumberOrExpression, term_repetition(Sign, Term))";
  the number of params in functor for the first optional construct in this rule ["+"|("-" nt)] must have 2 params since the option ("-" nt) has two elements.
  Variables in the template need not match the name of the non terminals on the rhs of a rule.
* */
object PrologTemplateProcessor extends ((List[BnFGrammarIR], PrologFactsBuilder) => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(elements: List[BnFGrammarIR], template: PrologFactsBuilder): List[BnFGrammarIR] =
    template.build(elements) match
      case Left(errorMsg) =>
        logger.error(errorMsg)
        ???
      case Right(prologFact) => ???

object GroupConstructProcessor extends (GroupConstruct => List[BnFGrammarIR]) with DeriveConstructs:
  override def apply(v1: GroupConstruct): List[BnFGrammarIR] = {
    check4PrologTemplate(v1) match
      case Some(prologTemplate) => PrologTemplateProcessor(v1.bnfObjects, prologTemplate)
      case None => v1.bnfObjects.filterNot(_.isInstanceOf[PrologFactsBuilder])
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
