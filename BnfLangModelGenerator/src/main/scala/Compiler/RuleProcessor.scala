/*
 Copyright (c) 7/23/23, 12:14 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{Nonterminal, NonterminalRegex, Terminal, Literal, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep}
import Utilz.CreateLogger


trait RuleProcessor(rule: Rule):
  val logger = CreateLogger(classOf[RuleProcessor])
  def extractMapping: Option[(String, List[BnFGrammarIR])]

class RuleContentProcessor(rule: Rule) extends RuleProcessor(rule):
  override def extractMapping: Option[(String, List[BnFGrammarIR])] =
    if rule.id.isInstanceOf[Terminal] then
      logger.error(s"Terminal ${rule.id} cannot be used to define a rule")
      None
    else Some((literalProcessing(rule.id)._2, null))

  private def literalProcessing(l: Literal): (IrLiteral, String) =
    l match
      case NonterminalRegex(id) => (RegExSpec(id), id)
      case Nonterminal(id) => (NT(id), id)
      case Terminal(id) => (T(id), id)

  private def content(rc: RuleContent): BnFGrammarIR = rc match
    case RuleLiteral(lit) => literalProcessing(lit)._1
    case RuleOpt(c) => ???
    case RuleRep(c) => ???
    case RuleGroup(c) => ???
    case RuleOr(c) => ???
    case RuleCollection(cc) => ???