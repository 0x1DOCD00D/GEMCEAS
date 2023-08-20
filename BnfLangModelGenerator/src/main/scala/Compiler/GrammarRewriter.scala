/*
 Copyright (c) 8/3/23, 12:01 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.GrammarRewriter.{logger, removeAllTerminals, removeAllUnions}
import Utilz.CreateLogger

import java.util.UUID
import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.util.Success

enum TerminationData:
  case INFINITELOOP
  case UNIONTERMINAL(branchIndex: Int)
  case DIRECTTERMINATION
  case INPROGRESS(path: List[UnionConstruct])

class GrammarRewriter(ast: List[ProductionRule]):
  import TerminationData.*
  def grammarConvergenceChecker(): List[BnFGrammarIR] = {
    ast.flatMap {
      rule =>
        rewriteRuleContent(List(rule.rhs), List(rule.rhs)) match
          case TerminationData.INFINITELOOP =>
            logger.info(s"Rule ${rule.lhs} doesn't terminate")
            List(rule.lhs)
          case ut@ TerminationData.UNIONTERMINAL(branchIndex) =>
            logger.error(s"${ut} cannot have union analyses results in this phase for ${ut.branchIndex}")
            List()
          case TerminationData.DIRECTTERMINATION => List()
          case TerminationData.INPROGRESS(path) =>
            if !rewriteUnion(path) then path else List()
    }
  }

  def findBnFObject(id: UUID): List[BnFGrammarIR] = {
    def findGrammarObject(go: BnFGrammarIR): List[BnFGrammarIR] = go match
      case container: BnFGrammarIRContainer => if container.uuid == id then List(container) else container.bnfObjects.flatMap(findGrammarObject(_))
      case literal: IrLiteral => if literal.uuid == id then List(literal) else List()
      case ProductionRule(lhs, rhs) => findGrammarObject(lhs) ::: findGrammarObject(rhs)
      case err =>
        logger.error(s"findGrammarObject run into a wrong object: $err")
        List()
    end findGrammarObject
    ast.flatMap(findGrammarObject(_))
  }

  def findGrammarObject(id: UUID, go: BnFGrammarIR): List[BnFGrammarIR] = go match
    case container: BnFGrammarIRContainer => if container.uuid == id then List(container) else container.bnfObjects.flatMap(findGrammarObject(id, _))
    case literal: IrLiteral => if literal.uuid == id then List(literal) else List()
    case ProductionRule(lhs, rhs) => findGrammarObject(id, lhs) ::: findGrammarObject(id, rhs)
    case err =>
      logger.error(s"findGrammarObject run into a wrong object: $err")
      List()

  private def rewriteUnion(ucl: List[UnionConstruct]): Boolean = {
    def checkUnion4Convergence(uc: UnionConstruct): Boolean = {
      uc.bnfObjects.foldLeft(false) {
        (acc, c) =>
          logger.info(s"Union's member ${c.uuid} -> ${c.toString}")
          rewriteRuleContent(List(c), List(c), true) match
            case TerminationData.INFINITELOOP => acc
            case ut@TerminationData.UNIONTERMINAL(branchIndex) =>
              logger.error(s"rewriting union $uc should not result in $ut")
              acc
            case TerminationData.DIRECTTERMINATION => acc | true
            case ip@TerminationData.INPROGRESS(path) =>
              logger.error(s"rewriting union $uc should not result in $ip")
              acc
      }
    }
    ucl.foldLeft(false)((acc, l) => acc | checkUnion4Convergence(l))
  }

  @tailrec
  private def rewriteRuleContent(rhs: List[BnFGrammarIR], acc: List[BnFGrammarIR], processUnions: Boolean = false): TerminationData = {
    import PartialFunction.cond
    logger.info(s"Content: ${rhs.mkString("; ")} and acc: ${acc.mkString("; ")}")
    val filteredOutTerminals: List[BnFGrammarIR] = removeAllTerminals(rhs)
    val filteredOutTermsAndUnions:List[BnFGrammarIR] = if processUnions then filteredOutTerminals else removeAllUnions(filteredOutTerminals)

    if filteredOutTerminals.length <= 0 then DIRECTTERMINATION
    else if filteredOutTermsAndUnions.length <= 0 && !processUnions then INPROGRESS(rhs.asInstanceOf)
    else
      val expandedRHS = rhs.flatMap {
        go =>
         val rw = deriveFromGrammarObject(go, processUnions)
         rw
      }
      logger.info(s"Expanded: ${expandedRHS.mkString("; ")}")
      val combined = if processUnions then removeAllTerminals(acc ::: expandedRHS) else removeAllTerminals(removeAllUnions(acc ::: expandedRHS))
      val infLoopCheck = combined.diff(combined.distinct)
      logger.info(s"Inf loop check: ${infLoopCheck.mkString("; ")}")
      if combined.map(_.uuid).diff(combined.map(_.uuid).distinct).nonEmpty then
        logger.info(s"Reached infinite loop")
        INFINITELOOP
      else rewriteRuleContent(expandedRHS, combined, processUnions)
  }

  @tailrec
  private def deriveFromGrammarObject(go: BnFGrammarIR, processUnion: Boolean = false): List[BnFGrammarIR] = {
    go match
      case container: OptionalConstruct => List()
      case container: RepeatConstruct => List()
      case container: GroupConstruct => container.bnfObjects
      case container: SeqConstruct => container.bnfObjects
      case container: UnionConstruct => if processUnion then container.bnfObjects else List(container)
      case lit @ BnfLiteral(token, ltype) if ltype == LiteralType.TERM || ltype == LiteralType.REGEXTERM || ltype == LiteralType.NTREGEX => List()
      case lit @ BnfLiteral(token, ltype) if ltype == LiteralType.NONTERM =>
        ast.find(_.lhs.asInstanceOf[BnfLiteral].token == lit.token) match
          case Some(rule) => deriveFromGrammarObject(rule.rhs)
          case None =>
            logger.error(s"Production rule for ${lit.toString} -> ${lit.uuid} is not found")
            List()
      case err =>
        logger.error(s"Unknown grammar object ${err.toString}")
        List()
  }

object GrammarRewriter:
  private val logger = CreateLogger(classOf[GrammarRewriter])
  private val removeAllTerminals: List[BnFGrammarIR] => List[BnFGrammarIR] = (lst: List[BnFGrammarIR]) => lst
    .filterNot(cond(_) { case BnfLiteral(_, LiteralType.NTREGEX) => true })
    .filterNot(cond(_) { case BnfLiteral(_, LiteralType.TERM) => true })
    .filterNot(cond(_) { case BnfLiteral(_, LiteralType.REGEXTERM) => true })
  val removeAllUnions: List[BnFGrammarIR] => List[BnFGrammarIR] = (lst: List[BnFGrammarIR]) => lst.filterNot(cond(_) { case UnionConstruct(_) => true } )

  @main def runMain_GrammarRewriter(): Unit =
    import Compiler.LiteralType.*
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    val grammar = List(
      ProductionRule(BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM))
          ))
        )
      ),
      ProductionRule(BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("+", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("-", TERM))
                    ))
                  ))
                ),
                BnfLiteral("product_div", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      product_div ::= ["+"|"-"] term {("*"|"/") term};
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("product_div", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              UnionConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("+", TERM))
                ),
                GroupConstruct(List(
                  BnfLiteral("-", TERM))
                ))
              ))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("*", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("/", TERM))
                    ))
                  ))
                ),
                BnfLiteral("term", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM))
            ))
          ))
        )
      ),
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral("""([+]|[-])?[0-9]+(\.[0-9]+)?""", REGEXTERM)
      )
    )
    grammar.foreach {
      rule =>
        logger.info(s"${rule.lhs.asInstanceOf[BnfLiteral].toString} -> ${rule.lhs.uuid.toString}")
    }
    val grw = new GrammarRewriter(grammar)
    val divergentNTs:List[BnFGrammarIR] = grw.grammarConvergenceChecker()
    if divergentNTs.isEmpty then logger.info("The grammar is convergent")
    else
      logger.error("The grammar is divergent")
      divergentNTs.foreach {
        nt => logger.error(s"Divergent NT: ${nt.uuid} -> ${nt.toString}")
      }
