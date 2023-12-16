/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Generator.DeriveConstructs
import Utilz.ConfigDb.*
import Utilz.Constants.{CloseKet, CommaSeparator, OpenBra}
import Utilz.{ConfigDb, PrologTemplate}

import java.util.UUID
import scala.annotation.tailrec

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
case class RepeatPrologFact(override val bnfObjects: List[BnFGrammarIR]) extends BnFGrammarIRContainer with CheckUpRewrite:
  def isRewriteCompleted: Boolean =
    bnfObjects.forall {
      case rfact: RepeatPrologFact => rfact.isRewriteCompleted
      case fact: PrologFact => fact.isRewriteCompleted
      case pe: ProgramEntity => true
      case _ => false
    }
  end isRewriteCompleted

  def formListOfBnFGrammarElements: List[BnFGrammarIR] = bnfObjects.flatMap {
    case rfact: RepeatPrologFact => rfact.formListOfBnFGrammarElements
    case fact: PrologFact => fact.formListOfBnFGrammarElements
    case pe: ProgramEntity => List(pe)
    case _ => List()
  }
  end formListOfBnFGrammarElements


case class PrologFact(functorName: String, mapParams2GrammarElements: List[(String, List[BnFGrammarIR])]) extends BnFGrammarIR with DeriveConstructs with CheckUpRewrite {
  final def rewriteGrammarElements(level: Int): Option[PrologFact] =
    def rewriteGrammarElement(acc: List[BnFGrammarIR], e: List[BnFGrammarIR]): List[BnFGrammarIR] =
      e match
        case ::(head, next) if head.isInstanceOf[PrologFact]  =>
          val headFact = head.asInstanceOf[PrologFact]
          if headFact.isRewriteCompleted then rewriteGrammarElement(head :: acc, next)
          else rewriteGrammarElement(head.asInstanceOf[PrologFact].rewriteGrammarElements(level + 1).getOrElse(ErrorInRewritingGrammarElements(level)) :: acc, next)
        case ::(head, next) if head.isInstanceOf[RepeatPrologFact]  =>
          val headFact = head.asInstanceOf[RepeatPrologFact]
          if headFact.isRewriteCompleted then rewriteGrammarElement(head :: acc, next)
          else
            val repeatedFacts = head.asInstanceOf[RepeatPrologFact].bnfObjects
            rewriteGrammarElement(RepeatPrologFact(rewriteGrammarElement(List(), repeatedFacts)) :: acc, next)
        case ::(head, next) if head.isInstanceOf[ProgramEntity] => rewriteGrammarElement(head :: acc, next)
        case ::(head, next) => rewriteGrammarElement(deriveElement(head, level > ConfigDb.`Gemceas.Generator.grammarMaxDepthRewriting`) ::: acc, next)
        case Nil => acc
    end rewriteGrammarElement

    if isRewriteCompleted then Some(this)
    else
      val rewritten: List[(String, List[BnFGrammarIR])] = mapParams2GrammarElements.foldLeft(List[(String, List[BnFGrammarIR])]()) {
        (acc, e) => (e._1, rewriteGrammarElement(List(), e._2)) :: acc
      }.map(e => (e._1, e._2.reverse))
      val fact: PrologFact = PrologFact(functorName, rewritten)

      if `Gemceas.Generator.debugProgramGeneration` then logger.info(s"Rewriting the Prolog fact $this at the level $level into new fact $fact")

      if level > grammarMaxDepthRewritingWithError then
        logger.error(s"Grammar unrolling has not been completed for the Prolog fact $fact at the level $level")
        None
      else fact.rewriteGrammarElements(level + 1)
  end rewriteGrammarElements

  //PrologFact(term,List((Number,List(ProgramEntity(-307)))))
  case class ErrorInRewritingGrammarElements(levelReached: Int) extends BnFGrammarIR
  def isRewriteCompleted: Boolean =
    mapParams2GrammarElements.flatMap(_._2).forall {
      case fact: PrologFact => fact.isRewriteCompleted
      case rfact: RepeatPrologFact => rfact.isRewriteCompleted
      case pe: ProgramEntity => true
      case err => false
    }

  def formListOfBnFGrammarElements: List[BnFGrammarIR] =
    mapParams2GrammarElements.flatMap(_._2).foldLeft(List[BnFGrammarIR]()) {
      (acc, e) => acc ::: (e match {
        case fact: PrologFact => fact.formListOfBnFGrammarElements
        case rfact: RepeatPrologFact => rfact.formListOfBnFGrammarElements
        case _ => List(e)
      })
    }

  def generatePrologFact4KBLS(top: Boolean): String =
    val listWrapper: List[String] => List[String] = (x: List[String]) =>
      if top then x
      else if x.isEmpty then List[String]().empty
      else if x.length == 1 then List(x.head)
      else
        val first = List(OpenBra + x.head)
        val last = List(x.reverse.head + CloseKet)
        first ::: x.slice(1, x.length-2) ::: last

    val params: List[String] = mapParams2GrammarElements.flatMap(_._2).foldLeft(List[String]()) {
      (acc, e) => acc ::: listWrapper(List(e match
        case fact: PrologFact => OpenBra + fact.generatePrologFact4KBLS(false) + CloseKet
        case pe: ProgramEntity => pe.code
        case _ => e.toString))
    }
    s"$functorName(${params.mkString(CommaSeparator.toString)})"
}

trait IrLiteral extends BnFGrammarIR
case class BnfLiteral(token: String, literalType: LiteralType) extends IrLiteral
case class PrologFactsBuilder(prt: PrologTemplate) extends IrLiteral {
//  xform factbuilder into an instance of PrologTemplate(functorName: String, params: List[PrologTemplate])
  def build(bnfObjects: List[BnFGrammarIR]): Either[String, PrologFact] =
    if prt.params.length != bnfObjects.length then Left(s"Number of parameters in the Prolog template ${prt.params} does not match the number of BNF objects $bnfObjects")
    else if prt.params.exists(_.params.nonEmpty) then Left(s"Prolog template ${prt.params} contains parameters with parameters")
    else Right(PrologFact(prt.functorName, prt.params.map(_.functorName).lazyZip(bnfObjects).toList.map(e => (e._1, List(e._2)))))
}
case class ProgramEntity(code: String) extends IrLiteral
case class IrError(err: String) extends BnFGrammarIR

object LocalTest:
  @main def runLocalTest(): Unit =
    val pf = PrologFact("sum_sub", List(("ProductDivRepetition",
      List(RepeatPrologFact(List(
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("049.21"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("-78.30"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("8.28"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("1.44"))))))))))))))))),
        PrologFact("product_div_repetition", List(("Sign", List(ProgramEntity("+"))),
          ("ProductDiv", List(PrologFact("product_div", List(("_", List()),
            ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("+464.33"))))))),
            ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
              ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("+3"))))))))))))))))))))),
      ("_", List(PrologFact("product_div", List(("_", List()),
        ("NumberOrExpression", List(PrologFact("term", List(("Number", List(ProgramEntity("-33.56"))))))),
        ("TermRepetition", List(PrologFact("term_repetition", List(("Sign", List(ProgramEntity("*"))),
          ("Term", List(PrologFact("term", List(("Number", List(ProgramEntity("-44")))))))))))))))))
    println(pf.isRewriteCompleted)