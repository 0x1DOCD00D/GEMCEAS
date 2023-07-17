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

import LexerParser.BnfGrammarAST.{MainRule, Nonterminal, NonterminalRegex, RuleContent, RHS}

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
enum BnfAttributeFlag:
  case REPETITIVE, OPTIONAL, NOATTR
end BnfAttributeFlag

enum BnfGrammarAST extends Positional:
  import BnfAttributeFlag.NOATTR
  case MainRule(aRule: Rule, otherRules: Option[MainRule])
  case Rule(id: Nonterminal | NonterminalRegex, rhs: RHS)
  case Nonterminal(id: String)
  case NonterminalRegex(id: String)
  case RHS(rc: Option[RuleContent], rhs: Option[RHS], flag: BnfAttributeFlag = BnfAttributeFlag.NOATTR)
  case RuleContent(term: Terminal, otherTerms: Option[RuleContent])
  case Terminal(id: String)
  case RegexString(id: String)
  case ISDEFINEDAS()
  case LESS()
  case GREATER()
  case VERTICALBAR()
  case BRA()
  case KET()
  case CURLYBRA()
  case CURLYKET()
  case DOUBLESLASH, SLASHSTAROPEN, COMMENT, NOTOKEN, PARSEFAILURE

case object NoToken extends Position:
  override def line: Int = -1

  override def column: Int = -1

  override protected def lineContents: String = "NO CONTENT AVAILABLE"
