/*
 Copyright (c) 7/24/23, 12:54 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{Literal, MainRule, Nonterminal, NonterminalRegex, RegexString, Rule, RuleCollection, RuleContent, RuleGroup, RuleLiteral, RuleOpt, RuleOr, RuleRep, Terminal}
import LiteralType.*
import Utilz.Constants.{MetaVariable_Assignment_Designator, Prolog_Template_Designator}
import Utilz.{CreateLogger, MetaVariableManager, PrologTemplateExtractor}

import scala.collection.mutable.ListBuffer

object AstExtractors:
  private val logger = CreateLogger(classOf[AstExtractors.type])

  private def flattenTreeOfLists(l: List[BnFGrammarIR], union: Boolean = false): List[BnFGrammarIR] =
  //      SeqConstruct(List(BnfLiteral(aWord,TERM), List(BnfLiteral(mainRule,NONTERM))))
  //      BnfLiteral(aWord,TERM), List(BnfLiteral(mainRule,NONTERM))
    l match
      case head :: next =>
        head match
          case construct: SeqConstruct => flattenTreeOfLists(construct.bnfObjects, union) ::: flattenTreeOfLists(next, union)
          case construct: UnionConstruct if union => List(GroupConstruct(flattenTreeOfLists(construct.bnfObjects, true) ::: flattenTreeOfLists(next, true)))
          case _ => List(head) ::: flattenTreeOfLists(next, union)
      case Nil => Nil
  end flattenTreeOfLists

  private def rewriteUnionization[T <: BnFGrammarIRContainer](sq: T, top: Boolean = false): List[BnFGrammarIRContainer] =
    val unionPresent: Boolean = sq.bnfObjects.count(_.isInstanceOf[UnionConstruct]) > 0
    val xformed: List[BnFGrammarIRContainer] =
      if unionPresent then
        val (unionConstructs, otherConstructs) = sq.bnfObjects.partition(_.isInstanceOf[UnionConstruct])
        GroupConstruct(otherConstructs) :: unionConstructs.foldLeft(List[BnFGrammarIRContainer]()) {
          (acc, uc) => acc ::: rewriteUnionization(uc.asInstanceOf[BnFGrammarIRContainer])
        }
      else List(GroupConstruct(sq.bnfObjects))
    if unionPresent && top then List(UnionConstruct(xformed)) else xformed
  end rewriteUnionization

  def apply(mr: MainRule): List[BnFGrammarIR] =
      mr.rules.map {
        r =>
          logger.info(s"Processing rule $r")
          val RuleExtractor(ir) = r : @unchecked
          ir
      }

  object RuleExtractor:
    def unapply(r: Rule): Option[ProductionRule] =
      val LiteralExtractor(ntid) = r.id : @unchecked
      ntid match
        case BnfLiteral(t, TERM) =>
          logger.error(s"Terminal $t cannot be used to define a rule")
          None
        case BnfLiteral(t, _) =>
          Some(t)
          r.rhs match
            case RuleLiteral(lit) =>
              val LiteralExtractor(v) = lit : @unchecked
              Option(ProductionRule(ntid, v))
            case rc @ RuleCollection(rcc) =>
              val RuleCollectionExtractor(rIr) = rc: @unchecked
              Option(ProductionRule(ntid, SeqConstruct(rewriteUnionization(SeqConstruct(flattenTreeOfLists(rIr.bnfObjects)), true))))
            case err =>
              logger.error(s"Rule ${err.toString}")
              None
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
        case NonterminalRegex(name) => Option(BnfLiteral(name.substring(name.indexOf('<')+1,name.indexOf('>')), NTREGEX))
        case RegexString(str) => Option(BnfLiteral(str, REGEXTERM))

  object RepeatExtractor:
    def unapply(l: RuleRep): Option[RepeatConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv : @unchecked
          Option(RepeatConstruct(rewriteUnionization(RepeatConstruct(flattenTreeOfLists(rCIr.bnfObjects)), true)))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the repeat modifier: error ${err.toString}")
          None

  object GroupExtractor:
    def unapply(l: RuleGroup): Option[GroupConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          Option(GroupConstruct(rewriteUnionization(GroupConstruct(flattenTreeOfLists(rCIr.bnfObjects)),true)))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the group modifier: error ${err.toString}")
          None

  object OptExtractor:
    def unapply(l: RuleOpt): Option[OptionalConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          Option(OptionalConstruct(rewriteUnionization(OptionalConstruct(flattenTreeOfLists(rCIr.bnfObjects)),true)))
        case err =>
          logger.error(s"Only RuleCollection can be specified under the group modifier: error ${err.toString}")
          None

  object UnionExtractor:
    def unapply(l: RuleOr): Option[UnionConstruct] =
      l.rc match
        case rcv@RuleCollection(rcc) =>
          val RuleCollectionExtractor(rCIr) = rcv: @unchecked
          val res = Option(UnionConstruct(flattenTreeOfLists(rCIr.bnfObjects)))
          res
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
          if PrologTemplateExtractor.isPrologTemplate(l.token).isDefined then
              val prologterm = PrologTemplateExtractor(l.token)
              if prologterm.isEmpty then
                logger.error(s"Failed to extract a prolog template from ${l.token}")
                IrError(s"Failed to extract a prolog template from ${l.token}")
              else PrologFactsBuilder(prologterm.get)
          else if MetaVariableManager.isMetaVariable(l.token) then
            val metaVar = MetaVariableManager(l.token)
            if metaVar.isEmpty then
              logger.error(s"Failed to extract a meta variable from ${l.token}")
              IrError(s"Failed to extract a meta variable from ${l.token}")
            else MetaVariable(metaVar.get._1, metaVar.get._2)
          else if l.token == MetaVariable_Assignment_Designator then
            logger.error(s"Failed to extract a meta variable assignment designator from ${l.token}")
            IrError(s"Failed to extract a meta variable assignment designator from ${l.token}")
          else if l.token == Prolog_Template_Designator then
            logger.error(s"Failed to extract a prolog template designator from ${l.token}")
            IrError(s"Failed to extract a prolog template designator from ${l.token}")
          else l
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
          val RuleCollectionExtractor(nested) = rc : @unchecked
          nested

    end extractIR

    def unapply(c: RuleCollection): Option[SeqConstruct] =
      val res = processTreeContent(c.rcc)
      Some(SeqConstruct(res))

end AstExtractors
