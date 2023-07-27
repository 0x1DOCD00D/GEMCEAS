/*
 Copyright (c) 7/26/23, 8:45 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.BnfGrammarCompiler
import LexerParser.{Nonterminal, *}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AstExtractorsTest extends AnyFlatSpec with Matchers {
  behavior of "the IR extractors"

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
    res shouldBe List()
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
    res shouldBe List()
  }
}