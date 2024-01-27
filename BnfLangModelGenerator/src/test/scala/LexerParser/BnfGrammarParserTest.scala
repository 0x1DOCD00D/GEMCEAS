/*
 Copyright (c) 7/21/23, 11:45 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package LexerParser

import Compiler.LoadGrammarFile.getClass
import Compiler.{BnfGrammarCompiler, LoadGrammarFile}
import Utilz.CreateLogger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

class BnfGrammarParserTest extends AnyFlatSpec with Matchers {
  behavior of "the (E)Bnf parser"

  val logger: Logger = CreateLogger(classOf[BnfGrammarParserTest])

  it should "parse a grammar with a simple rule that contains a terminal" in {
    val simpleGrammar =
      """
        | mainRule ::= "aWord" mainRule;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Terminal("aWord")),
          RuleCollection(List(
            RuleLiteral(Nonterminal("mainRule")))))
        )
      )
    ))
  }

  it should "parse a grammar with a simple rule that consists of a nonterminal" in {
    val simpleGrammar =
      """
        | mainRule ::= mainRule;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
          RuleCollection(List(
            RuleLiteral(Nonterminal("mainRule")))))
      )
    )
  }

  it should "parse a grammar with a simple union of a terminal and a nonterminal" in {
    val simpleGrammar =
      """
        | mainRule ::= y|"x"
        | ;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("y")),
          RuleCollection(List(
            RuleOr(
              RuleCollection(List(
                RuleLiteral(Terminal("x"))))
            ))))
        )))
    )
  }

  it should "parse a grammar with a simple union of seven nonterminals" in {
    val simpleGrammar =
      """
        | mainRule ::= a b | c d e | f g | z
        | ;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("a")),
          RuleCollection(List(
            RuleLiteral(Nonterminal("b")),
            RuleCollection(List(
              RuleOr(
                RuleCollection(List(
                  RuleLiteral(Nonterminal("c")),
                  RuleCollection(List(RuleLiteral(Nonterminal("d")),
                    RuleCollection(List(RuleLiteral(Nonterminal("e")),
                      RuleCollection(List(
                        RuleOr(
                          RuleCollection(List(
                            RuleLiteral(Nonterminal("f")),
                            RuleCollection(List(
                              RuleLiteral(Nonterminal("g")),
                              RuleCollection(List(
                                RuleOr(
                                  RuleCollection(List(
                                    RuleLiteral(Nonterminal("z"))
                                  ))
                                ))
                              ))
                            ))
                          )
                        )
                      ))
                    ))
                  ))
                ))
              )))
          ))
        ))
      ))
    )
  }


  it should "parse a grammar with a union of a nonterminal, an option and a repeat" in {
    val simpleGrammar =
      """
        | mainRule ::= x | ["y" z] | {v w} | theRestOfIt
        | ;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("x")),
          RuleCollection(List(
            RuleOr(
              RuleCollection(List(
                RuleOpt(
                  RuleCollection(List(
                    RuleLiteral(Terminal("y")),
                    RuleCollection(List(
                      RuleLiteral(Nonterminal("z")))
                    ))
                  )
                ),
                RuleCollection(List(
                  RuleOr(
                    RuleCollection(List(
                      RuleRep(
                        RuleCollection(List(
                          RuleLiteral(Nonterminal("v")),
                          RuleCollection(List(
                            RuleLiteral(Nonterminal("w"))))))),
                      RuleCollection(List(
                        RuleOr(
                          RuleCollection(List(
                            RuleLiteral(Nonterminal("theRestOfIt")))
                          )
                        ))
                      ))
                    )
                  ))
                )))
            ))
          ))
        )))
    )
  }

  it should "parse a grammar with a repeat of a terminal and an nt" in {
    val simpleGrammar =
      """
        | mainRule ::= {"x" y z}
        | ;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(Rule(Nonterminal("mainRule"),
      RuleCollection(List(
        RuleRep(
          RuleCollection(List(
            RuleLiteral(Terminal("x")),
            RuleCollection(List(RuleLiteral(Nonterminal("y")), RuleCollection(List(RuleLiteral(Nonterminal("z"))))))
          )
          )
        ))
      )))
    )
  }

  it should "parse a grammar with a simple repeat of a terminal" in {
    val simpleGrammar =
      """
        | mainRule ::= {"x"}
        | ;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleLiteral(Terminal("x"))))
          )
        ))
      ))
    )
  }

  it should "parse a grammar with a // comment" in {
    val simpleGrammar =
      """
        | //this is a single rule
        | mainRule ::= mainRule;
        | // concluding comment here
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
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
                RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Nonterminal("done"))))))
                )))))
              )))))
            )))))
          )
        ))
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3")))))))))
        )),
      Rule(Nonterminal("nt1"),RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
        RuleCollection(List(
          RuleOpt(RuleCollection(List(
            RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")))))
          )))
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3")))))))))
        )
      ),
      Rule(Nonterminal("nt1"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(
            RuleRep(RuleCollection(List(
              RuleLiteral(Nonterminal("nt2")),
              RuleCollection(List(RuleLiteral(Terminal("x"))))))
            ))
          ))
        )
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x")),
              RuleCollection(List(RuleLiteral(Nonterminal("nt3"))))))))))
      ),
      Rule(Nonterminal("nt1"),
        RuleCollection(List(RuleLiteral(Nonterminal("nt1")),
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
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
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
            ))))
        )),
      Rule(Nonterminal("rule"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("mainRule")),
          RuleCollection(List(
            RuleOr(RuleCollection(List(
              RuleOpt(
                RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(RuleLiteral(Nonterminal("done"))))))
              )))
            ))))
        )
      ),
      Rule(Nonterminal("done"),
        RuleCollection(List(
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
              RuleCollection(List(RuleLiteral(Nonterminal("done"))))))))
        ))
      )
    )
  }

  it should "parse a grammar with a combination of unionized constructs" in {
    val simpleGrammar =
      """
        | mainRule ::= [{a} (b | c)] | d | e f;
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"), RuleCollection(List(
        RuleOpt(RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleLiteral(Nonterminal("a"))))
          ),
          RuleCollection(List(
            RuleGroup(RuleCollection(List(
              RuleLiteral(Nonterminal("b")),
              RuleCollection(List(
                RuleOr(RuleCollection(List(
                  RuleLiteral(Nonterminal("c"))))
                )))))
            ))
          )))
        ),
        RuleCollection(List(
          RuleOr(RuleCollection(List(
            RuleLiteral(Nonterminal("d")),
            RuleCollection(List(
              RuleOr(RuleCollection(List(
                RuleLiteral(Nonterminal("e")),
                RuleCollection(List(
                  RuleLiteral(Nonterminal("f")))
                )))
              ))
            )))
          ))
        )))
      ))
    )
  }

  it should "parse a grammar with a combination of groupped constructs" in {
    val simpleGrammar =
      """
        | mainRule ::= ((a) (b (c))) (d (e) f);
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(simpleGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleGroup(
            RuleCollection(List(
              RuleGroup(
                RuleCollection(List(
                  RuleLiteral(Nonterminal("a")))
                )
              ),
              RuleCollection(List(
                RuleGroup(
                  RuleCollection(List(
                    RuleLiteral(Nonterminal("b")),
                    RuleCollection(List(
                      RuleGroup(
                        RuleCollection(List(
                          RuleLiteral(Nonterminal("c")))
                        )
                      ))
                    )))
                ))
              ))
            )
          ),
          RuleCollection(List(
            RuleGroup(
              RuleCollection(List(
                RuleLiteral(Nonterminal("d")),
                RuleCollection(List(
                  RuleGroup(
                    RuleCollection(List(
                      RuleLiteral(Nonterminal("e")))
                    )
                  ),
                  RuleCollection(List(
                    RuleLiteral(Nonterminal("f")))
                  ))
                ))
              )
            ))
          ))
        )
      ))
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
    val ast = BnfGrammarCompiler.parseGrammar(expGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("expression"), RuleCollection(List(RuleLiteral(Nonterminal("sum_sub"))))),
      Rule(Nonterminal("sum_sub"), RuleCollection(List(
        RuleLiteral(Nonterminal("product_div")),
        RuleCollection(List(
          RuleRep(RuleCollection(List(RuleGroup(RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(
            RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))),
            RuleCollection(List(RuleLiteral(Nonterminal("product_div"))))))
          ))
        )))
      ),
      Rule(Nonterminal("product_div"), RuleCollection(List(
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
      Rule(Nonterminal("term"), RuleCollection(List(RuleLiteral(Nonterminal("number")), RuleCollection(List(
        RuleOr(
          RuleCollection(List(RuleLiteral(Terminal("(")),
            RuleCollection(List(RuleLiteral(Nonterminal("expression")),
              RuleCollection(List(RuleLiteral(Terminal(")"))))))))
        )))))
      ),
      Rule(NonterminalRegex("<number>"), RuleLiteral(RegexString("""(\+|\-)?[0-9]+(\.[0-9]+)?"""))))
    )
  }

  it should "parse a divergent expression grammar" in {
    val expGrammar =
      """
        |expression ::= sum_sub;
        |sum_sub ::= product_div {("+"|"-") product_div};
        |product_div ::= ["+"|"-"] term {("*"|"/") number};
        |term ::= sum_sub | "(" expression ")";
        |<number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
        |""".stripMargin
    val ast = BnfGrammarCompiler.parseGrammar(expGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("expression"),
        RuleCollection(List(RuleLiteral(Nonterminal("sum_sub"))))),
      Rule(Nonterminal("sum_sub"),
        RuleCollection(List(RuleLiteral(Nonterminal("product_div")),
          RuleCollection(List(RuleRep(RuleCollection(List(RuleGroup(RuleCollection(List(RuleLiteral(Terminal("+")),
            RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))),
            RuleCollection(List(RuleLiteral(Nonterminal("product_div")))))))))))),
      Rule(Nonterminal("product_div"),
        RuleCollection(List(RuleOpt(RuleCollection(List(RuleLiteral(Terminal("+")),
          RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))),
          RuleCollection(List(RuleLiteral(Nonterminal("term")),
            RuleCollection(List(RuleRep(RuleCollection(List(RuleGroup(RuleCollection(List(RuleLiteral(Terminal("*")),
              RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("/")))))))))),
              RuleCollection(List(RuleLiteral(Nonterminal("number")))))))))))))),
      Rule(Nonterminal("term"),
        RuleCollection(List(RuleLiteral(Nonterminal("sum_sub")),
          RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("(")),
            RuleCollection(List(RuleLiteral(Nonterminal("expression")),
              RuleCollection(List(RuleLiteral(Terminal(")")))))))))))))), Rule(NonterminalRegex("<number>"),
        RuleLiteral(RegexString("""(\+|\-)?[0-9]+(\.[0-9]+)?""")))))
  }

  it should "parse an expression grammar with prolog templates from resources" in {
    /*
    expression ::=
      sum_sub
      "==>> expression(SumSub)";

    sum_sub ::=
      product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
      "==>> sum_sub(_, ProductDivRepetition)";

    product_div ::=
      ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
      "==>> product_div(_, NumberOrExpression, TermRepetition)";

    term ::=
      number
      "==>> term(Number)" |
      "(" expression ")"
      "==>> term(_, Expression, _)";

    <number> ::=
      "[\-\+]?[0-9]+(\.[0-9]+)?";
   */
    val grammarFilePath = System.getProperty("user.dir") + "/src/main/resources/Grammars/ArithmeticExpressions.bnf"
    val source = scala.io.Source.fromFile(grammarFilePath)

    val srcGrammar: String = try source.mkString finally source.close()
    if srcGrammar.isEmpty then
      logger.error("Failed to load a grammar, terminating Gemceas.")
      System.exit(1)
    else logger.info(srcGrammar)
    srcGrammar.isEmpty shouldBe false
    val ast = BnfGrammarCompiler.parseGrammar(srcGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("expression"), RuleCollection(List(
        RuleLiteral(Nonterminal("sum_sub")), RuleCollection(List(
          RuleLiteral(Terminal("==>> expression(SumSub)"))))))
      ),
      Rule(Nonterminal("sum_sub"), RuleCollection(List(
        RuleLiteral(Nonterminal("product_div")), RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleGroup(RuleCollection(List(
              RuleLiteral(Terminal("+")),
              RuleCollection(List(
                RuleOr(RuleCollection(List(
                  RuleLiteral(Terminal("-")))))))))
            ),
            RuleCollection(List(
              RuleLiteral(Nonterminal("product_div")),
              RuleCollection(List(
                RuleLiteral(Terminal("==>> product_div_repetition(Sign, ProductDiv)"))))))))
          ),
          RuleCollection(List(
            RuleLiteral(Terminal("==>> sum_sub(_, ProductDivRepetition)"))))))))
      ),
      Rule(Nonterminal("product_div"), RuleCollection(List(
        RuleOpt(RuleCollection(List(
          RuleLiteral(Terminal("+")),
          RuleCollection(List(
            RuleOr(RuleCollection(List(
              RuleLiteral(Terminal("-")))))))))
        ),
        RuleCollection(List(
          RuleLiteral(Nonterminal("term")),
          RuleCollection(List(
            RuleRep(RuleCollection(List(
              RuleGroup(RuleCollection(List(
                RuleLiteral(Terminal("*")),
                RuleCollection(List(
                  RuleOr(RuleCollection(List(
                    RuleLiteral(Terminal("/")))))))))
              ),
              RuleCollection(List(
                RuleLiteral(Nonterminal("term")),
                RuleCollection(List(
                  RuleLiteral(Terminal("==>> term_repetition(Sign, Term)"))))))))
            ),
            RuleCollection(List(
              RuleLiteral(Terminal("==>> product_div(_, NumberOrExpression, TermRepetition)")))
            ))
          ))
        )))
      ),
      Rule(Nonterminal("term"), RuleCollection(List(
        RuleLiteral(Nonterminal("number")),
        RuleCollection(List(
          RuleLiteral(Terminal("==>> term(Number)")),
          RuleCollection(List(
            RuleOr(RuleCollection(List(
              RuleLiteral(Terminal("(")),
              RuleCollection(List(
                RuleLiteral(Nonterminal("expression")), RuleCollection(List(
                  RuleLiteral(Terminal(")")), RuleCollection(List(
                    RuleLiteral(Terminal("==>> term(_, Expression, _)")))))))
              )))
            ))
          ))
        )))
      ),
      Rule(NonterminalRegex("<number>"),
        RuleLiteral(RegexString("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?""")))))
  }

  it should "parse an expression grammar with prolog templates and metavariables from resources" in {
    /*
   expression ::=
    sum_sub
    "==>> expression(SumSub)";

    sum_sub ::=
      product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
      "==>> sum_sub(_, ProductDivRepetition)";

    product_div ::=
      ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
      "PrevProductDiv =:= sum_sub.product_div._1"
      "==>> product_div(PrevProductDiv, _, NumberOrExpression, TermRepetition)";

    term ::=
      number
      "PrevTerm =:= product_div.term._2"
      "==>> term(PrevTerm, Number)" |
      "(" expression ")"
      "==>> term(_, Expression, _)";

    <number> ::=
      "[\-\+]?[0-9]{1,3}(\.[0-9]{2})?";
   * */
    val grammarFilePath = System.getProperty("user.dir") + "/src/main/resources/Grammars/ArithmeticExpressionsWithMetavars.bnf"
    val source = scala.io.Source.fromFile(grammarFilePath)

    val srcGrammar: String = try source.mkString finally source.close()
    if srcGrammar.isEmpty then
      logger.error("Failed to load a grammar, terminating Gemceas.")
      System.exit(1)
    else logger.info(srcGrammar)
    srcGrammar.isEmpty shouldBe false
    val ast = BnfGrammarCompiler.parseGrammar(srcGrammar).getOrElse(MainRule(List()))
    ast shouldBe MainRule(List(
      Rule(Nonterminal("expression"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("sum_sub")),
          RuleCollection(List(
            RuleLiteral(Terminal("==>> expression(SumSub)"))))))
      ),
      Rule(Nonterminal("sum_sub"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("product_div")),
          RuleCollection(List(
            RuleRep(
              RuleCollection(List(
                RuleGroup(
                  RuleCollection(List(
                    RuleLiteral(Terminal("+")),
                    RuleCollection(List(
                      RuleOr(
                        RuleCollection(List(
                          RuleLiteral(Terminal("-")))))))))
                ),
                RuleCollection(List(
                  RuleLiteral(Nonterminal("product_div")),
                  RuleCollection(List(
                    RuleLiteral(
                      Terminal("==>> product_div_repetition(Sign, ProductDiv)")
                    )))))))
            ),
            RuleCollection(List(
              RuleLiteral(Terminal("==>> sum_sub(_, ProductDivRepetition)")))
            )))))
      ),
      Rule(Nonterminal("product_div"),
        RuleCollection(List(
          RuleOpt(RuleCollection(List(RuleLiteral(Terminal("+")), RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))),
          RuleCollection(List(
            RuleLiteral(Nonterminal("term")),
            RuleCollection(List(
              RuleRep(
                RuleCollection(List(
                  RuleGroup(
                    RuleCollection(List(
                      RuleLiteral(Terminal("*")),
                      RuleCollection(List(
                        RuleOr(
                          RuleCollection(List(
                            RuleLiteral(Terminal("/")))))))))
                  ),
                  RuleCollection(List(
                    RuleLiteral(Nonterminal("term")),
                    RuleCollection(List(
                      RuleLiteral(Terminal("==>> term_repetition(Sign, Term)")))))
                  )))
              ),
              RuleCollection(List(
                RuleLiteral(Terminal("PrevProductDiv =:= sum_sub.product_div._1")),
                RuleCollection(List(
                  RuleLiteral(Terminal("==>> product_div(PrevProductDiv, _, NumberOrExpression, TermRepetition)")))))
              )))))))
      ),
      Rule(Nonterminal("term"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("number")),
          RuleCollection(List(
            RuleLiteral(Terminal("PrevTerm =:= product_div.term._2")),
            RuleCollection(List(
              RuleLiteral(Terminal("==>> term(PrevTerm, Number)")),
              RuleCollection(List(
                RuleOr(
                  RuleCollection(List(
                    RuleLiteral(Terminal("(")),
                    RuleCollection(List(
                      RuleLiteral(Nonterminal("expression")),
                      RuleCollection(List(
                        RuleLiteral(Terminal(")")),
                        RuleCollection(List(
                          RuleLiteral(Terminal("==>> term(_, Expression, _)")))))))))))))))))))
      ),
      Rule(NonterminalRegex("<number>"), RuleLiteral(RegexString("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?"""))))
    )
  }
}
