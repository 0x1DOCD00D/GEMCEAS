/*
 Copyright (c) 7/26/23, 8:45 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.BnfGrammarCompiler
import LexerParser.{Nonterminal, *}
import LiteralType.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AstExtractorsTest extends AnyFlatSpec with Matchers {
  behavior of "the IR extractors"

  it should "extract an IR representation from an optional rule with a collection of elements" in {
    //    nt2 ::= [nt1 nt2 "x"];
    val parsedGrammar = MainRule(List(Rule(Nonterminal("nt2"),
      RuleCollection(List(
        RuleOpt(RuleCollection(List(
          RuleLiteral(Nonterminal("nt1")),
          RuleCollection(List(RuleLiteral(Nonterminal("nt2")),
            RuleCollection(List(RuleLiteral(Terminal("x"))))))))
        )))
    ))
    )

    val res = AstExtractors(parsedGrammar)
    res shouldBe List(ProductionRule(BnfLiteral("nt2", NONTERM),
      SeqConstruct(List(
        OptionalConstruct(List(BnfLiteral("nt1", NONTERM), BnfLiteral("nt2", NONTERM), BnfLiteral("x", TERM)))
      ))
    ))
  }

  it should "extract an IR representation from a repeat rule with a collection of elements" in {
//    mainRule ::= {"x" y z}
    val parsedGrammar = MainRule(List(Rule(Nonterminal("mainRule"),
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

    val res = AstExtractors(parsedGrammar)
    res shouldBe List(
      ProductionRule(BnfLiteral("mainRule", NONTERM),
        SeqConstruct(List(
          RepeatConstruct(List(
            BnfLiteral("x", TERM), BnfLiteral("y", NONTERM), BnfLiteral("z", NONTERM))
          ))
        )
      )
    )
  }

  it should "extract an IR representation from a repeat rule with a single terminal" in {
    //     mainRule ::= {"x"}
    val parsedGrammar = MainRule(List(
      Rule(Nonterminal("mainRule"),
        RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleLiteral(Terminal("x"))))
          )
        ))
      ))
    )
    val res = AstExtractors(parsedGrammar)
    res shouldBe List(ProductionRule(BnfLiteral("mainRule", NONTERM), SeqConstruct(List(RepeatConstruct(List(BnfLiteral("x", TERM)))))))
  }

  it should "extract an IR representation from a sequence rule" in {
//    mainRule ::= "aWord" mainRule;
    val parsedGrammar = MainRule(List(
      Rule(
        Nonterminal("mainRule"),
        RuleCollection(List(
          RuleLiteral(Terminal("aWord")),
          RuleCollection(List(
            RuleLiteral(Nonterminal("mainRule")))))
        )
      )
    ))
    val res = AstExtractors(parsedGrammar)
    res shouldBe List(ProductionRule(BnfLiteral("mainRule", NONTERM), SeqConstruct(List(BnfLiteral("aWord", TERM), BnfLiteral("mainRule", NONTERM)))))
  }

  it should "extract an IR representation from a unionized rule" in {
//    mainRule ::= x | ["y" z] | {v w} | theRestOfIt
    val parsedGrammar = MainRule(List(
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
    val res = AstExtractors(parsedGrammar)
    //    mainRule ::= x | ["y" z] | {v w} | theRestOfIt
    res shouldBe List(ProductionRule(
      BnfLiteral("mainRule", NONTERM),
      UnionConstruct(List(
        BnfLiteral("x", NONTERM),
        OptionalConstruct(List(BnfLiteral("y", TERM), BnfLiteral("z", NONTERM))),
        RepeatConstruct(List(BnfLiteral("v", NONTERM), BnfLiteral("w", NONTERM))),
        BnfLiteral("theRestOfIt", NONTERM))
      ))
    )
  }
}