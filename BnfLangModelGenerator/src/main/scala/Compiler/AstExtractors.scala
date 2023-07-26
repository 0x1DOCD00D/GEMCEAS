/*
 Copyright (c) 7/24/23, 12:54 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{Nonterminal, NonterminalRegex, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep, Terminal}
import LiteralType.*
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

object AstExtractors:
  private val logger = CreateLogger(classOf[AstExtractors.type])

  object RuleExtractor:
    def unapply(r: Rule): Option[BnFGrammarIrMap] =
      val LiteralExtractor(ntid) = r.id : @unchecked
      val ruleId:Option[String] = ntid match
        case BnfLiteral(t, TERM) =>
          logger.error(s"Terminal $t cannot be used to define a rule")
          None
        case BnfLiteral(t, _) => Some(t)
      if ruleId.nonEmpty then
        r.rhs match
          case RuleLiteral(lit) =>
            val LiteralExtractor(v) = lit : @unchecked
            Option(Map(ruleId.get -> List(v)))
          case RuleCollection(rcc) =>
            val subRules = rcc.map {
              sr =>
                val RuleCollectionExtractor(rIr) = sr: @unchecked
                rIr
            }
            Option(Map(ruleId.get -> subRules))
          case err =>
            logger.error(s"Rule ${err.toString}")
            None
      else None


  object LiteralExtractor:
    def unapply(l: RuleLiteral): Option[BnfLiteral] =
      l.lit match
        case Terminal(name) => Option(BnfLiteral(name, TERM))
        case Nonterminal(name) => Option(BnfLiteral(name, NONTERM))
        case NonterminalRegex(name) => Option(BnfLiteral(name, NTREGEX))
        case err =>
          logger.error(s"Unknown type of literal, $err")
          None

  object RepeatExtractor:
    def unapply(l: RuleRep): Option[RepeatConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv : @unchecked
          Option(RepeatConstruct(rCIr))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the repeat modifier: error ${err.toString}")
          None

  object GroupExtractor:
    def unapply(l: RuleGroup): Option[GroupConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          Option(GroupConstruct(rCIr))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the group modifier: error ${err.toString}")
          None

  object OptExtractor:
    def unapply(l: RuleOpt): Option[OptionalConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          Option(OptionalConstruct(rCIr))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the group modifier: error ${err.toString}")
          None

  object RuleCollectionExtractor:
    def unapply(c: RuleContent): Option[BnFGrammarIR] =
      c match
        case RuleLiteral(lit) => ???
        case RuleOpt(rc) => ???
        case RuleRep(rc) => ???
        case RuleGroup(rc) => ???
        case RuleOr(rc) => ???
        case RuleCollection(rcc) => ???