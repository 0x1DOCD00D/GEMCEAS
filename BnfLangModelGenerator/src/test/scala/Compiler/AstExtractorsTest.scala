/*
 Copyright (c) 7/26/23, 8:45 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{Nonterminal, *}
import LiteralType.*
import Utilz.PrologTemplate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AstExtractorsTest extends AnyFlatSpec with Matchers {
  behavior of "the IR extractors"

  it should "extract an IR representation from a union rule with a collection of elements" in {
    //      mainRule ::= a b | c d e | f g | z
    val parsedGrammar = MainRule(List(
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

    val res = AstExtractors(parsedGrammar)
    //      mainRule ::= a b | c d e | f g | z
    res shouldBe List(ProductionRule(
      BnfLiteral("mainRule", NONTERM),
      SeqConstruct(List(
        UnionConstruct(List(
          GroupConstruct(List(
            BnfLiteral("a", NONTERM), BnfLiteral("b", NONTERM))
          ),
          GroupConstruct(List(
            BnfLiteral("c", NONTERM), BnfLiteral("d", NONTERM), BnfLiteral("e", NONTERM))
          ),
          GroupConstruct(List(
            BnfLiteral("f", NONTERM), BnfLiteral("g", NONTERM))
          ),
          GroupConstruct(List(
            BnfLiteral("z", NONTERM))
          ))
        ))
      ))
    )
  }

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
    res shouldBe List(ProductionRule(
      BnfLiteral("nt2", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          OptionalConstruct(List(
            GroupConstruct(List(
              BnfLiteral("nt1", NONTERM), BnfLiteral("nt2", NONTERM), BnfLiteral("x", TERM))
            ))
          ))
        ))
      ))
    )
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
    res shouldBe List(ProductionRule(
      BnfLiteral("mainRule", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          RepeatConstruct(List(
            GroupConstruct(List(BnfLiteral("x", TERM), BnfLiteral("y", NONTERM), BnfLiteral("z", NONTERM)))
          ))
        )))
      ))
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
    res shouldBe List(ProductionRule(BnfLiteral("mainRule", NONTERM),
      SeqConstruct(List(
        GroupConstruct(List(
          RepeatConstruct(List(
            GroupConstruct(List(
              BnfLiteral("x", TERM))
            ))
          ))
        ))
      ))
    )
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
    res shouldBe List(
      ProductionRule(
        BnfLiteral("mainRule", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("aWord", TERM),
            BnfLiteral("mainRule", NONTERM))
          ))
        )
      )
    )
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
    res shouldBe List[ProductionRule](
      ProductionRule(
        BnfLiteral("mainRule", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("x", NONTERM))
            ),
            GroupConstruct(List(
              OptionalConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("y", TERM), BnfLiteral("z", NONTERM))
                ))
              ))
            ),
            GroupConstruct(List(
              RepeatConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("v", NONTERM), BnfLiteral("w", NONTERM))
                ))
              ))
            ),
            GroupConstruct(List(
              BnfLiteral("theRestOfIt", NONTERM))
            ))
          )))
      )
    )
  }

  it should "extract an IR representation from a nested unionized rule" in {
//    mainRule ::=[ {a}(b | c) ] | d | e f;
    val parsedGrammar = MainRule(List(
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

    val res = AstExtractors(parsedGrammar)
    //     mainRule ::= [{a} (b | c)] | d | e f;
    res shouldBe List(ProductionRule(
      BnfLiteral("mainRule", NONTERM),
      SeqConstruct(List(
        UnionConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              GroupConstruct(List(
                RepeatConstruct(List(
                  GroupConstruct(List(BnfLiteral("a", NONTERM)))
                )
                ),
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("b", NONTERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("c", NONTERM))
                    ))
                  ))
                ))
              ))
            ))
          ),
          GroupConstruct(List(
            BnfLiteral("d", NONTERM))
          ),
          GroupConstruct(List(
            BnfLiteral("e", NONTERM), BnfLiteral("f", NONTERM))
          ))
        ))
      ))
    )
  }

  it should "extract an IR representation from the expression grammar" in {
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    val parsedGrammar = MainRule(List(
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
      Rule(Nonterminal("term"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("number")),
          RuleCollection(List(
            RuleOr(
              RuleCollection(List(RuleLiteral(Terminal("(")),
                RuleCollection(List(RuleLiteral(Nonterminal("expression")),
                  RuleCollection(List(RuleLiteral(Terminal(")"))))))))
            ))
          ))
        )
      ),
      Rule(NonterminalRegex("<number>"), RuleLiteral(RegexString("""(\+|\-)?[0-9]+(\.[0-9]+)?"""))))
    )

    val res = AstExtractors(parsedGrammar)
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    res shouldBe List(
      ProductionRule(BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM))
          ))
        )
      ),
      ProductionRule(BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("+", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("-", TERM))
                    ))
                  ))
                ),
                BnfLiteral("product_div", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      product_div ::= ["+"|"-"] term {("*"|"/") term};
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("product_div", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              UnionConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("+", TERM))
                ),
                GroupConstruct(List(
                  BnfLiteral("-", TERM))
                ))
              ))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("*", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("/", TERM))
                    ))
                  ))
                ),
                BnfLiteral("term", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM))
            ))
          ))
        )
      ),
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral("""(\+|\-)?[0-9]+(\.[0-9]+)?""", REGEXTERM)
      )
    )
  }

  it should "extract an IR representation from an incorrect expression grammar" in {
    val parsedGrammar = MainRule(List(
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

    val res = AstExtractors(parsedGrammar)
    res shouldBe List(ProductionRule(BnfLiteral("expression", NONTERM), SeqConstruct(List(GroupConstruct(List(BnfLiteral("sum_sub", NONTERM)))))), ProductionRule(BnfLiteral("sum_sub", NONTERM), SeqConstruct(List(GroupConstruct(List(BnfLiteral("product_div", NONTERM), RepeatConstruct(List(GroupConstruct(List(GroupConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("+", TERM))), GroupConstruct(List(BnfLiteral("-", TERM))))))), BnfLiteral("product_div", NONTERM)))))))))), ProductionRule(BnfLiteral("product_div", NONTERM), SeqConstruct(List(GroupConstruct(List(OptionalConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("+", TERM))), GroupConstruct(List(BnfLiteral("-", TERM))))))), BnfLiteral("term", NONTERM), RepeatConstruct(List(GroupConstruct(List(GroupConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("*", TERM))), GroupConstruct(List(BnfLiteral("/", TERM))))))), BnfLiteral("number", NONTERM)))))))))), ProductionRule(BnfLiteral("term", NONTERM), SeqConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))), GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM)))))))), ProductionRule(BnfLiteral("number", NTREGEX), BnfLiteral("""(\+|\-)?[0-9]+(\.[0-9]+)?""", REGEXTERM)))
  }

  it should "extract an IR representation from an exp grammar with prolog templates" in {
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
      "(\+|\-)?[0-9]+(\.[0-9]+)?";
   */
    val expGrammar = MainRule(List(
      Rule(Nonterminal("expression"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("sum_sub")), RuleCollection(List(
            RuleLiteral(Terminal("==>> expression(SumSub)"))))))
      ),
      Rule(Nonterminal("sum_sub"), RuleCollection(List(
        RuleLiteral(Nonterminal("product_div")), RuleCollection(List(
          RuleRep(RuleCollection(List(
            RuleGroup(RuleCollection(List(
              RuleLiteral(Terminal("+")),
              RuleCollection(List(
                RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))
            ),
            RuleCollection(List(
              RuleLiteral(Nonterminal("product_div")), RuleCollection(List(
                RuleLiteral(Terminal("==>> product_div_repetition(Sign, ProductDiv)"))))))))
          ),
          RuleCollection(List(
            RuleLiteral(Terminal("==>> sum_sub(_, ProductDivRepetition)")))
          )))))
      ),
      Rule(Nonterminal("product_div"),
        RuleCollection(List(
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
                RuleLiteral(Terminal("==>> product_div(_, NumberOrExpression, TermRepetition)"))))))))))
      ),
      Rule(Nonterminal("term"),
        RuleCollection(List(
          RuleLiteral(Nonterminal("number")),
          RuleCollection(List(
            RuleLiteral(Terminal("==>> term(Number)")),
            RuleCollection(List(
              RuleOr(RuleCollection(List(
                RuleLiteral(Terminal("(")),
                RuleCollection(List(
                  RuleLiteral(Nonterminal("expression")),
                  RuleCollection(List(
                    RuleLiteral(Terminal(")")),
                    RuleCollection(List(
                      RuleLiteral(Terminal("==>> term(_, Expression, _)")))))))))))))))))
      ),
      Rule(NonterminalRegex("<number>"),
        RuleLiteral(RegexString("""(\+|\-)?[0-9]+(\.[0-9]+)?""")))
    ))
    val res = AstExtractors(expGrammar)
    res shouldBe List(
      /*
        expression ::=
          sum_sub
          "==>> expression(SumSub)";
      * */
      ProductionRule(BnfLiteral("expression", NONTERM), SeqConstruct(List(
        GroupConstruct(List(
          BnfLiteral("sum_sub", NONTERM),
          PrologFactsBuilder(PrologTemplate("expression", List(PrologTemplate("SumSub", List())))))
        )))
      ),
      /*
        sum_sub ::=
        product_div {("+"|"-") product_div "==>> product_div_repetition(Sign, ProductDiv)"}
        "==>> sum_sub(_, ProductDivRepetition)";
      * */
      ProductionRule(BnfLiteral("sum_sub", NONTERM), SeqConstruct(List(
        GroupConstruct(List(
          BnfLiteral("product_div", NONTERM),
          RepeatConstruct(List(
            GroupConstruct(List(
              GroupConstruct(List(
                UnionConstruct(List(
                  GroupConstruct(List(BnfLiteral("+", TERM))),
                  GroupConstruct(List(BnfLiteral("-", TERM))))))
              ),
              BnfLiteral("product_div", NONTERM),
              PrologFactsBuilder(PrologTemplate("product_div_repetition", List(
                PrologTemplate("Sign", List()), PrologTemplate("ProductDiv", List())))
              ))))
          ),
          PrologFactsBuilder(PrologTemplate("sum_sub", List(
            PrologTemplate("_", List()),
            PrologTemplate("ProductDivRepetition", List())))
          )))))
      ),
      /*
        product_div ::=
          ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
          "==>> product_div(_, NumberOrExpression, TermRepetition)";
      * */
      ProductionRule(BnfLiteral("product_div", NONTERM), SeqConstruct(List(
        GroupConstruct(List(
          OptionalConstruct(List(
            UnionConstruct(List(
              GroupConstruct(List(BnfLiteral("+", TERM))),
              GroupConstruct(List(BnfLiteral("-", TERM))))
            ))
          ),
          BnfLiteral("term", NONTERM),
          RepeatConstruct(List(
            GroupConstruct(List(
              GroupConstruct(List(
                UnionConstruct(List(
                  GroupConstruct(List(BnfLiteral("*", TERM))),
                  GroupConstruct(List(BnfLiteral("/", TERM))))
                ))
              ),
              BnfLiteral("term", NONTERM),
              PrologFactsBuilder(PrologTemplate("term_repetition", List(
                PrologTemplate("Sign", List()),
                PrologTemplate("Term", List())))
              ))))
          ),
          PrologFactsBuilder(PrologTemplate("product_div", List(
            PrologTemplate("_", List()),
            PrologTemplate("NumberOrExpression", List()),
            PrologTemplate("TermRepetition", List())))
          )))))
      ),
      /*
        term ::=
          number
          "==>> term(Number)" |
          "(" expression ")"
          "==>> term(_, Expression, _)";
      * */
      ProductionRule(BnfLiteral("term", NONTERM), SeqConstruct(List(
        UnionConstruct(List(
          GroupConstruct(List(
            BnfLiteral("number", NONTERM),
            PrologFactsBuilder(PrologTemplate("term", List(
              PrologTemplate("Number", List())))
            ))
          ),
          GroupConstruct(List(
            BnfLiteral("(", TERM),
            BnfLiteral("expression", NONTERM),
            BnfLiteral(")", TERM),
            PrologFactsBuilder(PrologTemplate("term", List(
              PrologTemplate("_", List()),
              PrologTemplate("Expression", List()),
              PrologTemplate("_", List())))
            )))))))
      ),
      /*
        <number> ::=
          "(\+|\-)?[0-9]+(\.[0-9]+)?";
      * */
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral("""(\+|\-)?[0-9]+(\.[0-9]+)?""", REGEXTERM)
      )
    )
  }

  it should "extract an IR representation from an exp grammar with prolog templates and metavariables" in {
    val expGrammar = MainRule(List(
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
      Rule(NonterminalRegex("<number>"), RuleLiteral(RegexString("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?""")))
    )
    )
    val res = AstExtractors(expGrammar)
    res shouldBe List(
      ProductionRule(
        BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM),
            PrologFactsBuilder(PrologTemplate("expression", List(PrologTemplate("SumSub", List())))))))
        )
      ),
      ProductionRule(
        BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(BnfLiteral("+", TERM))),
                    GroupConstruct(List(BnfLiteral("-", TERM))))))
                ),
                BnfLiteral("product_div", NONTERM),
                PrologFactsBuilder(PrologTemplate("product_div_repetition", List(PrologTemplate("Sign", List()), PrologTemplate("ProductDiv", List())))))))
            ),
            PrologFactsBuilder(PrologTemplate("sum_sub", List(PrologTemplate("_", List()), PrologTemplate("ProductDivRepetition", List())))))))
        )
      ),
      ProductionRule(
        BnfLiteral("product_div", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              UnionConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("+", TERM))
                ),
                GroupConstruct(List(
                  BnfLiteral("-", TERM))
                ))))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(BnfLiteral("*", TERM))),
                    GroupConstruct(List(BnfLiteral("/", TERM))))))
                ),
                BnfLiteral("term", NONTERM),
                PrologFactsBuilder(PrologTemplate("term_repetition", List(
                  PrologTemplate("Sign", List()),
                  PrologTemplate("Term", List())))))))
            ),
            MetaVariable("PrevProductDiv", List("sum_sub", "product_div", "_1")),
            PrologFactsBuilder(PrologTemplate("product_div", List(
              PrologTemplate("PrevProductDiv", List()),
              PrologTemplate("_", List()),
              PrologTemplate("NumberOrExpression", List()),
              PrologTemplate("TermRepetition", List()))))))))
      ),
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM),
              MetaVariable("PrevTerm", List("product_div", "term", "_2")),
              PrologFactsBuilder(PrologTemplate("term", List(PrologTemplate("PrevTerm", List()), PrologTemplate("Number", List())))))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM),
              PrologFactsBuilder(PrologTemplate("term", List(
                PrologTemplate("_", List()),
                PrologTemplate("Expression", List()),
                PrologTemplate("_", List()))))))))))
      ),
      ProductionRule(BnfLiteral("number", NTREGEX), BnfLiteral("""[\-\+]?[0-9]{1,3}(\.[0-9]{2})?""", REGEXTERM)))
  }
}