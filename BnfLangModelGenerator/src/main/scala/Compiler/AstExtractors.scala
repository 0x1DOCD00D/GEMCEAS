/*
 Copyright (c) 7/24/23, 12:54 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{Literal, MainRule, Nonterminal, NonterminalRegex, RegexString, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep, Terminal}
import LiteralType.*
import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

object AstExtractors:
  private val logger = CreateLogger(classOf[AstExtractors.type])

  def apply(mr: MainRule): List[BnFGrammarIR] =
    mr.rules.map {
      r =>
        val RuleExtractor(ir) = r : @unchecked
        ir
    }

  object RuleExtractor:
    def flattenTreeOfLists(l: List[BnFGrammarIR]): List[BnFGrammarIR] =
//      SeqConstruct(List(BnfLiteral(aWord,TERM), List(BnfLiteral(mainRule,NONTERM))))
//      BnfLiteral(aWord,TERM), List(BnfLiteral(mainRule,NONTERM))
      l match
        case head :: next  =>
          if head.isInstanceOf[SeqConstruct] then
            head.asInstanceOf[SeqConstruct].bnfObjects ::: flattenTreeOfLists(next)
          else List(head) ::: flattenTreeOfLists(next)
        case Nil => Nil
    end flattenTreeOfLists

    def unapply(r: Rule): Option[BnFGrammarIR] =
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
            Option(v)
          case rc @ RuleCollection(rcc) =>
            val RuleCollectionExtractor(rIr) = rc: @unchecked
            val res = Option(SeqConstruct(flattenTreeOfLists(rIr.bnfObjects)))
            res
          case err =>
            logger.error(s"Rule ${err.toString}")
            None
      else None
    end unapply
  end RuleExtractor

  object LiteralExtractor:
    def unapply(l: RuleLiteral): Option[BnfLiteral] =
      val LiteralExtractor(bl) = l.lit : @unchecked
      Option(bl)

    def unapply(rl: Literal): Option[BnfLiteral] =
      rl match
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

  object UnionExtractor:
    private def flattenUnionStructure(or: RuleOr): UnionConstruct =
      ???
    end flattenUnionStructure

    def unapply(l: RuleOr): Option[UnionConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          Option(UnionConstruct(List(rCIr)))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the group modifier: error ${err.toString}")
          None

  object RuleCollectionExtractor:
    private def processTreeContent(rcc: List[RuleContent]): List[BnFGrammarIR] =
      rcc match
        case ::(head, tl) => extractIR(head) :: processTreeContent(tl)
        case Nil => Nil
    end processTreeContent

    private def extractIR(c: RuleContent): BnFGrammarIR =
      c match
        case rlit @ RuleLiteral(lit) =>
          val LiteralExtractor(l) = rlit : @unchecked
          l
        case ro @ RuleOpt(rc) =>
          val OptExtractor(o) = ro : @unchecked
          o
        case rrep @ RuleRep(rc) =>
          val RepeatExtractor(r) = rrep: @unchecked
          r
        case rgrp @ RuleGroup(rc) =>
          val GroupExtractor(g) = rgrp: @unchecked
          g

        case or @ RuleOr(rc) =>
          val UnionExtractor(un) = or: @unchecked
          un

        case rc @ RuleCollection(rcc) =>
          val RuleCollectionExtractor(nested) = rc
          nested

    end extractIR

    def unapply(c: RuleCollection): Option[SeqConstruct] =
      val res = processTreeContent(c.rcc)
      Some(SeqConstruct(res))

