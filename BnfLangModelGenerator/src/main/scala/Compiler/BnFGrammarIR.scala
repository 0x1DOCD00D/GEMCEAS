/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Generator.{DeriveConstructs, DerivationTree}
import Utilz.ConfigDb.*
import Utilz.Constants.{CloseKet, CommaSeparator, OpenBra}
import Utilz.{ConfigDb, PrologTemplate}

import java.util.UUID
import scala.annotation.tailrec

sealed trait BnFGrammarIR:
  val uuid: UUID = UUID.randomUUID()
  def replicaWithUniqueID: BnFGrammarIR = this match
    case pr: ProductionRule => pr.copy(uuid = UUID.randomUUID())
    case oc: OptionalConstruct => oc.copy(uuid = UUID.randomUUID())
    case rc: RepeatConstruct => rc.copy(uuid = UUID.randomUUID())
    case gc: GroupConstruct => gc.copy(uuid = UUID.randomUUID())
    case sc: SeqConstruct => sc.copy(uuid = UUID.randomUUID())
    case uc: UnionConstruct => uc.copy(uuid = UUID.randomUUID())
    case rp: RepeatPrologFact => rp.copy(uuid = UUID.randomUUID())
    case pf: PrologFact => pf.copy(uuid = UUID.randomUUID())
    case ml: MetaVariable => MetaVariable(ml.name, ml.path)
    case bl: BnfLiteral => bl.copy(uuid = UUID.randomUUID())
    case pe: ProgramEntity => pe.copy(uuid = UUID.randomUUID())
    case ir: IrError => ir
    case unknownError =>
      logger.error(s"Unknown BNF grammar element $unknownError")
      unknownError

trait BnFGrammarIRContainer extends BnFGrammarIR:
  val bnfObjects: List[BnFGrammarIR]
end BnFGrammarIRContainer

type BnFGrammarIrMap = Map[String, List[BnFGrammarIR]]
type MetaVarMap = Map[String, List[BnFGrammarIR]]

enum LiteralType:
  case TERM, NONTERM, NTREGEX, REGEXTERM

case class ProductionRule(lhs: BnFGrammarIR, rhs: BnFGrammarIR, override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIR:
  override def toString: String = s"ProductionRule($lhs, $rhs)"
case class OptionalConstruct(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer:
  override def toString: String = s"OptionalConstruct($bnfObjects)"
case class RepeatConstruct(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer:
  override def toString: String = s"RepeatConstruct($bnfObjects)"
case class GroupConstruct(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer:
  override def toString: String = s"GroupConstruct($bnfObjects)"
case class SeqConstruct(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer:
  override def toString: String = s"SeqConstruct($bnfObjects)"
case class UnionConstruct(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer:
  override def toString: String = s"UnionConstruct($bnfObjects)"
case class RepeatPrologFact(override val bnfObjects: List[BnFGrammarIR], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIRContainer with CheckUpRewrite with GeneratePrologFact:
  override def toString: String = s"RepeatPrologFact($bnfObjects)"
  def formListOfBnFGrammarElements: List[BnFGrammarIR] = bnfObjects.flatMap {
    case rfact: RepeatPrologFact => rfact.formListOfBnFGrammarElements
    case fact: PrologFact => fact.formListOfBnFGrammarElements
    case pe: ProgramEntity => List(pe)
    case _ => List()
  }
  end formListOfBnFGrammarElements

case class PrologFact(functorName: String, mapParams2GrammarElements: List[(String, List[BnFGrammarIR])], override val uuid: UUID = UUID.randomUUID()) extends BnFGrammarIR with DeriveConstructs with CheckUpRewrite with GeneratePrologFact {
  final def rewriteGrammarElements(level: Int): Option[PrologFact] =
    def rewriteGrammarElement(acc: List[BnFGrammarIR], e: List[BnFGrammarIR]): List[BnFGrammarIR] =
      e match
        case ::(head, next) if head.isInstanceOf[PrologFact]  =>
          val headFact = head.asInstanceOf[PrologFact]
          if headFact.isRewriteCompleted() then rewriteGrammarElement(head :: acc, next)
          else rewriteGrammarElement(head.asInstanceOf[PrologFact].rewriteGrammarElements(level + 1).getOrElse(ErrorInRewritingGrammarElements(level)) :: acc, next)
        case ::(head, next) if head.isInstanceOf[RepeatPrologFact]  =>
          val headFact = head.asInstanceOf[RepeatPrologFact]
          if headFact.isRewriteCompleted() then rewriteGrammarElement(head :: acc, next)
          else
            val repeatedFacts = head.asInstanceOf[RepeatPrologFact].bnfObjects
            rewriteGrammarElement(RepeatPrologFact(rewriteGrammarElement(List(), repeatedFacts)) :: acc, next)
        case ::(head, next) if head.isInstanceOf[ProgramEntity] =>
          DerivationTree.addGrammarElements(List(), head, 1)
          rewriteGrammarElement(head :: acc, next)
        case ::(head, next) =>
          val gels = deriveElement(head, level > `Gemceas.Generator.grammarMaxDepthRewriting`)
          DerivationTree.addGrammarElements(gels, head, 1)
          rewriteGrammarElement(gels ::: acc, next)
        case Nil => acc
    end rewriteGrammarElement

    if isRewriteCompleted() then Some(this)
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
  def formListOfBnFGrammarElements: List[BnFGrammarIR] =
    mapParams2GrammarElements.flatMap(_._2).foldLeft(List[BnFGrammarIR]()) {
      (acc, e) => acc ::: (e match {
        case fact: PrologFact => fact.formListOfBnFGrammarElements
        case rfact: RepeatPrologFact => rfact.formListOfBnFGrammarElements
        case _ => List(e)
      })
    }
}

trait IrLiteral extends BnFGrammarIR

case object ParameterSkipped extends IrLiteral
case object NonExistentElement extends BnFGrammarIR

case class MetaVariable(name: String, path: List[String], override val uuid: UUID = UUID.randomUUID()) extends IrLiteral:
  override def toString: String = s"MetaVariable($name, $path)"
case class BnfLiteral(token: String, literalType: LiteralType, override val uuid: UUID = UUID.randomUUID()) extends IrLiteral:
  override def toString: String = s"BnfLiteral($token, $literalType)"
case class PrologFactsBuilder(prt: PrologTemplate) extends IrLiteral {
//  xform factbuilder into an instance of PrologTemplate(functorName: String, params: List[PrologTemplate])
  override def toString: String = s"PrologFactsBuilder($PrologTemplate)"
  def build(bnfObjects: List[BnFGrammarIR]): Either[String, PrologFact] =
    if prt.params.length != bnfObjects.length then Left(s"Number of parameters in the Prolog template ${prt.params} does not match the number of BNF objects $bnfObjects")
    else if prt.params.exists(_.params.nonEmpty) then Left(s"Prolog template ${prt.params} contains parameters with parameters")
    else Right(PrologFact(prt.functorName, prt.params.map(_.functorName).lazyZip(bnfObjects).toList.map(e => (e._1, List(e._2)))))
}
case class ProgramEntity(code: String, @transient override val uuid: UUID = UUID.randomUUID()) extends IrLiteral
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
    println(pf.isRewriteCompleted())