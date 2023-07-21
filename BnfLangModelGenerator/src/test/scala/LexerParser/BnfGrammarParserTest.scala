/*
 Copyright (c) 7/21/23, 11:45 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package LexerParser

import Compiler.BnfGrammarCompiler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BnfGrammarParserTest extends AnyFlatSpec with Matchers {
  behavior of "the (E)Bnf parser"

  it should "parse a grammar with a simple rule that contains a terminal" in {
    val simpleGrammar =
      """
        | mainRule ::= "aWord" mainRule;
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Terminal("\"aWord\"")),
          RuleCollection(List(
            RuleLiteral(Nonterminal("mainRule"))))))
        )
      )
    )
  }

  it should "parse a grammar with a simple rule that consists of a nonterminal" in {
    val simpleGrammar =
      """
        | mainRule ::= mainRule;
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
          RuleCollection(List(
            RuleLiteral(Nonterminal("mainRule")))))
      )
    )
  }

  it should "parse a grammar with a // comment" in {
    val simpleGrammar =
      """
        | //this is a single rule
        | mainRule ::= mainRule;
        | // concluding comment here
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("mainRule")))))
    )
    )
  }

  it should "parse a grammar with a multiline comment" in {
    val simpleGrammar =
      """
        | /*
        |  this is a multiple
        |  line comment*/
        | mainRule ::= mainRule;
        | /*
        |  this is a
        |   concluding multiple
        |     line comment
        |   // and it is an ignored comment
        |  */
        """.stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("mainRule")))))
    )
    )
  }
}
