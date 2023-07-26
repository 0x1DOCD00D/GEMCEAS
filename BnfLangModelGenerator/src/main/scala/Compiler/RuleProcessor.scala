/*
 Copyright (c) 7/23/23, 12:14 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.AstExtractors.LiteralExtractor
import LexerParser.{Literal, Nonterminal, NonterminalRegex, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep, Terminal}
import Utilz.CreateLogger
import org.slf4j.Logger


trait RuleProcessor(rule: Rule):
  protected val logger: Logger = CreateLogger(classOf[RuleProcessor])
  def extractMapping: Option[(String, List[BnFGrammarIR])]

class RuleContentProcessor(rule: Rule) extends RuleProcessor(rule):
  override def extractMapping: Option[(String, List[BnFGrammarIR])] =
    if rule.id.isInstanceOf[Terminal] then
      logger.error(s"Terminal ${rule.id} cannot be used to define a rule")
      None
    else Some((null,null))
    ???

  private def content(rc: RuleContent): BnFGrammarIR =
    rc match
      case lt @ RuleLiteral(_) =>
        val LiteralExtractor(bnfl) = lt : @unchecked
        bnfl
      case RuleOpt(c) => ???
      case RuleRep(c) => ???
      case RuleGroup(c) => ???
      case RuleOr(c) => ???
      case RuleCollection(cc) => ???
