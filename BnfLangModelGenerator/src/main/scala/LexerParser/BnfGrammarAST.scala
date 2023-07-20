/*******************************************************************************
 *
 *  Copyright (c) 7/11/23, 2:53 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

package LexerParser

import scala.util.matching.Regex
import scala.util.parsing.input.{Position, Positional}

/*
 mainRule           ::= rule | rule mainRule
 rule               ::= rule-name_regex "::=" rhs
 <rule-name>        ::= "[<a-zA-Z][-#>$\.:_a-zA-Z0-9]*"
 rhs                ::= ruleContent | ruleContent "|" rhs | "[" rhs "]" | "{" rhs "}"
 ruleContent        ::= term | term ruleContent
 term               ::= <literal> | <rule-name>
 <literal>          ::= "\"[-#$~\.:_a-zA-Z0-9]*\"" | "\'[_a-zA-Z][-#$\.:_a-zA-Z0-9]*\'" term               ::= <literal> | <rule-name>
 <literal>          ::= "\".*\"" | "\'\.*\'"
 */

sealed trait BnfGrammarAST extends Positional

case class MainRule(rules: List[Rule]) extends BnfGrammarAST

case class Rule(id: Literal, rhs: RuleContent) extends BnfGrammarAST

sealed trait RuleContent extends BnfGrammarAST
case class RuleLiteral(lit: Literal) extends RuleContent
case class RuleOpt(rc: RuleContent) extends RuleContent
case class RuleRep(rc: RuleContent) extends RuleContent
case class RuleOr(rc: RuleContent) extends RuleContent
case class RuleCollection(rcc: List[RuleContent]) extends RuleContent
case class PARSEFAILURE(err: String) extends BnfGrammarAST
