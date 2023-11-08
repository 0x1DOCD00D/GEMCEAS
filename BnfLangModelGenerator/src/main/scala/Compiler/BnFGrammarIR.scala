/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Utilz.PrologTemplate

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
case class PrologFact(functorName: String, mapParams2GrammarElements: Map[String, List[BnFGrammarIR]]) extends BnFGrammarIR {
  def rewriteGrammarElement(entry: List[(String, List[BnFGrammarIR])]): PrologFact =
    PrologFact(functorName, entry.toMap)

  def formListOfBnFGrammarElements: List[BnFGrammarIR] =
    mapParams2GrammarElements.values.toList.flatten.foldLeft(List[BnFGrammarIR]()) {
      (acc, e) => acc ::: (e match {
        case fact: PrologFact => fact.formListOfBnFGrammarElements
        case _ => List(e)
      })
    }

  def generatePrologFact4KBLS(top: Boolean): String =
    val listWrapper: List[String] => List[String] = (x: List[String]) =>
      if top then x
      else
        val first = List("[" + x.head)
        val last = List(x.reverse.head + "]")
        first ::: x.slice(1, x.length - 1) ::: last

    val params: List[String] = mapParams2GrammarElements.values.toList.flatten.foldLeft(List[String]()) {
      (acc, e) => acc ::: listWrapper(List(e match
        case fact: PrologFact => fact.generatePrologFact4KBLS(false)
        case _ => e.toString))
    }
    s"$functorName(${params.mkString(",")})"
}

trait IrLiteral extends BnFGrammarIR
case class BnfLiteral(token: String, literalType: LiteralType) extends IrLiteral
case class PrologFactsBuilder(prt: PrologTemplate) extends IrLiteral {
  def build(bnfObjects: List[BnFGrammarIR]): Either[String, PrologFact] =
    if prt.params.length != bnfObjects.length then Left(s"Number of parameters in the Prolog template ${prt.params} does not match the number of BNF objects $bnfObjects")
    else if prt.params.filter(_.params.nonEmpty).nonEmpty then Left(s"Prolog template ${prt.params} contains parameters with parameters")
    else Right(PrologFact(prt.functorName, prt.params.map(_.functorName).lazyZip(bnfObjects).toList.groupMap(_._1)(_._2)))
}
case class ProgramEntity(code: String) extends IrLiteral
case class IrError(err: String) extends BnFGrammarIR
