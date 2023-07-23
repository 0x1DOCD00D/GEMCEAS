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
          RuleLiteral(Terminal("aWord")),
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

  it should "parse a grammar with a three rules containing OR" in {
    val simpleGrammar =
      """
        | mainRule  ::= nt_regex rule;
        | rule      ::= mainRule | "+" done
        | ;
        | done      ::= "a" | "b" | "c" | rule | done   ;
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt_regex")), RuleCollection(List(RuleLiteral(Nonterminal("rule"))))))
      ),
      Rule(Nonterminal("rule"),
        RuleCollection(List(RuleLiteral(Nonterminal("mainRule")),
          RuleCollection(List(RuleOr(
            RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(RuleLiteral(Nonterminal("done")))))))
          ))))
      ),
      Rule(Nonterminal("done"),
        RuleCollection(List(RuleLiteral(Terminal("a")),
          RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("b")),
            RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("c")),
              RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Nonterminal("rule")),
                RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Nonterminal("done"))))))))))))))))))))))))
      ))
    )
  }

  it should "parse a grammar with rules that contain optional constructs []" in {
    val simpleGrammar =
      """
        | mainRule ::= nt1 nt2 "x" nt3;
        | nt1 ::= nt1 [nt2 "x"];
        | nt2 ::= [nt1 nt2 "x"];
        | nt3 ::= [[nt1 nt2] "x"];
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3"))))))))))),
      Rule(Nonterminal("nt1"),RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
        RuleCollection(List(
          RuleOpt(RuleCollection(List(
            RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))
        )))
      ),
      Rule(Nonterminal("nt2"),
        RuleCollection(List(
          RuleOpt(RuleCollection(List(
            RuleLiteral(Nonterminal("nt1")),
            RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
              RuleCollection(List(RuleLiteral(Terminal("x"))))))))
          )))
      ),
      Rule(Nonterminal("nt3"),
        RuleCollection(List(
          RuleOpt(RuleCollection(List(
            RuleOpt(RuleCollection(List(
              RuleLiteral(Nonterminal("nt1")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt2"))))))
            ),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))))))
  }

  it should "parse a grammar with rules with reps {}" in {
    val simpleGrammar =
      """
        | mainRule ::= nt1 nt2 "x" nt3;
        | nt1 ::= nt1 {nt2 "x"};
        | nt2 ::= {nt1 nt2 "x"};
        | nt3 ::= {{nt1 nt2} "x"};
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3"))))))))))),
      Rule(Nonterminal("nt1"), RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
        RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))
        )))
      ),
      Rule(Nonterminal("nt2"),
        RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleLiteral(Nonterminal("nt1")),
            RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
              RuleCollection(List(RuleLiteral(Terminal("x"))))))))
          )))
      ),
      Rule(Nonterminal("nt3"),
        RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleRep(RuleCollection(List(
              RuleLiteral(Nonterminal("nt1")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt2"))))))
            ),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))))))
  }

  it should "parse a grammar with rules containing groups ()" in {
    val simpleGrammar =
      """
        | mainRule ::= nt1 nt2 "x" nt3;
        | nt1 ::= nt1 (nt2 "x");
        | nt2 ::= (nt1 nt2 "x");
        | nt3 ::= ((nt1 nt2) "x");
        | """.stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3"))))))))))),
      Rule(Nonterminal("nt1"), RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
        RuleCollection(List(
          RuleGroup(RuleCollection(List(
            RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))
        )))
      ),
      Rule(Nonterminal("nt2"),
        RuleCollection(List(
          RuleGroup(RuleCollection(List(
            RuleLiteral(Nonterminal("nt1")),
            RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
              RuleCollection(List(RuleLiteral(Terminal("x"))))))))
          )))
      ),
      Rule(Nonterminal("nt3"),
        RuleCollection(List(
          RuleGroup(RuleCollection(List(
            RuleGroup(RuleCollection(List(
              RuleLiteral(Nonterminal("nt1")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt2"))))))
            ),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))))))
  }

  it should "parse a grammar with a three rules containing OR and {} and [] and ()" in {
    val simpleGrammar =
      """
        | mainRule  ::= nt_regex (rule [do "it"]);
        | rule      ::= mainRule | ["+" done]
        | ;
        | done      ::= {"a" | ("b" | "c" | rule)} | done   ;
        |""".stripMargin
    val ast = BnfGrammarCompiler(simpleGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("nt_regex")),
          RuleCollection(List(
            RuleGroup(RuleCollection(List(RuleLiteral(Nonterminal("rule")),
              RuleCollection(List(
                RuleOpt(RuleCollection(List(
                  RuleLiteral(Nonterminal("do")), RuleCollection(List(RuleLiteral(Terminal("it"))))))
                )))))
            )))))),
      Rule(Nonterminal("rule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("mainRule")), RuleCollection(List(
            RuleOr(RuleCollection(List(
              RuleOpt(
                RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(RuleLiteral(Nonterminal("done"))))))
              )))
            )))))),
      Rule(Nonterminal("done"),RuleCollection(List(
        RuleRep(
          RuleCollection(List(
            RuleLiteral(Terminal("a")),
            RuleCollection(List(
              RuleOr(RuleCollection(List(
                RuleGroup(RuleCollection(List(RuleLiteral(Terminal("b")),
                  RuleCollection(List(
                    RuleOr(RuleCollection(List(RuleLiteral(Terminal("c")),
                      RuleCollection(List(
                        RuleOr(
                          RuleCollection(List(RuleLiteral(Nonterminal("rule"))))
                        )))))
                    ))))))))
              )))))
        ),
        RuleCollection(List(
          RuleOr(
            RuleCollection(List(RuleLiteral(Nonterminal("done")))))))))))
    )
  }

  it should "parse an expression grammar" in {
    val expGrammar =
      """
        |expression ::= sum_sub;
        |sum_sub ::= product_div {("+"|"-") product_div};
        |product_div ::= ["+"|"-"] term {("*"|"/") term};
        |term ::= number | "(" expression ")";
        |<number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
        |""".stripMargin
    val ast = BnfGrammarCompiler(expGrammar)
    ast shouldBe MainRule(List(
      Rule(Nonterminal("expression"),RuleCollection(List(RuleLiteral(Nonterminal("sum_sub"))))),
      Rule(Nonterminal("sum_sub"),RuleCollection(List(
        RuleLiteral(Nonterminal("product_div")),
        RuleCollection(List(
          RuleRep(RuleCollection(List(RuleGroup(RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(
            RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))),
            RuleCollection(List(RuleLiteral(Nonterminal("product_div"))))))))
        )))
      ),
      Rule(Nonterminal("product_div"),RuleCollection(List(
        RuleOpt(RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(
          RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))
        ),
        RuleCollection(List(RuleLiteral(Nonterminal("term")),
          RuleCollection(List(
            RuleRep(RuleCollection(List(
              RuleGroup(RuleCollection(List(RuleLiteral(Terminal("*")), RuleCollection(List(
                RuleOr(RuleCollection(List(RuleLiteral(Terminal("/")))))))))
              ),
              RuleCollection(List(RuleLiteral(Nonterminal("term"))))))))))
        )))
      ),
      Rule(Nonterminal("term"),RuleCollection(List(RuleLiteral(Nonterminal("number")), RuleCollection(List(
        RuleOr(
          RuleCollection(List(RuleLiteral(Terminal("(")),
            RuleCollection(List(RuleLiteral(Nonterminal("expression")),
              RuleCollection(List(RuleLiteral(Terminal(")"))))))))
        )))))
      ),
      Rule(NonterminalRegex("<number>"),RuleLiteral(RegexString("""(\+|\-)?[0-9]+(\.[0-9]+)?"""))))
    )
  }

}
