/*
 Copyright (c) 7/22/23, 7:01 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.AstExtractors.LiteralExtractor
import LexerParser.{BnfGrammarAST, MainRule, Nonterminal, NonterminalRegex, PARSEFAILURE, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep}
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

class AstAnalyzer:
  private val logger = CreateLogger(classOf[AstAnalyzer])
  private val terminals: Array[String] = Array.empty
  def checkMainRule(mr: MainRule): Either[IrError, List[Rule]] =
    if !mr.rules.forall(_.isInstanceOf[Rule]) then Left(IrError("MainRule contains non rules."))
    else Right(mr.rules)
  end checkMainRule

  def checkRule(r: Rule): Either[IrError, (String, RuleContent)] =
    if r.id.isInstanceOf[Nonterminal] || r.id.isInstanceOf[NonterminalRegex] then
      val name: String =
        if r.id.isInstanceOf[Nonterminal] then r.id.asInstanceOf[Nonterminal].id
        else r.id.asInstanceOf[NonterminalRegex].id
      checkContent(r.rhs) match
        case Left(err) => Left(err)
        case Right(c) => Right((name, c))
    else Left(IrError(s"Rule is defined as ${r.id} that is neither nt nor nt regex."))
  end checkRule

  def checkContent(content: RuleContent): Either[IrError, RuleContent] =
    content match
      case rl@RuleLiteral(lit) =>
//        val ltral = LiteralExtractor(rl)
        ???
      case RuleOpt(rc) => ???
      case RuleRep(rc) => ???
      case RuleGroup(rc) => ???
      case RuleOr(rc) => ???
      case RuleCollection(rcc) =>
        if rcc.count(_.isInstanceOf[RuleOr]) > 0 then
          ???
        ???
  end checkContent

end AstAnalyzer


object AstAnalyzer:
  def apply(ast: BnfGrammarAST): Either[IrError, BnFGrammarIrMap] =
    def constructIr(lOr: List[Rule]): Either[IrError, BnFGrammarIrMap] =
      val ir =  lOr.map {
        rule => null

      }
      ???
    end constructIr

    ast match
      case MainRule(rules) => constructIr(rules)
      case PARSEFAILURE(err) => Left(IrError(err))
      case err => Left(IrError(s"Ast should have MainRule as the root, not ${err.toString}"))
  end apply


  @main def runMain_AstAnalyzer(): Unit =
    println(AstAnalyzer.getClass.getName)
