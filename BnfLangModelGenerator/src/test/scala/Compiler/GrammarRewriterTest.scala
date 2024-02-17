/*
 Copyright (c) 8/18/23, 5:36 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Compiler.BnfGrammarCompiler
import LexerParser.{Nonterminal, *}
import LiteralType.*
import Utilz.CreateLogger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

import java.util.UUID
import scala.collection.immutable.List

class GrammarRewriterTest extends AnyFlatSpec with Matchers {
  behavior of "the grammar rewriting mechanism"

  val logger: Logger = CreateLogger(classOf[GrammarRewriterTest])
  val convergentGrammar: List[ProductionRule] = List(
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
      BnfLiteral("""([+]|[-])?[0-9]+(\.[0-9]+)?""", REGEXTERM)
    )
  )

  val divergentGrammar: List[ProductionRule] = List(ProductionRule(BnfLiteral("expression", NONTERM), SeqConstruct(List(GroupConstruct(List(BnfLiteral("sum_sub", NONTERM)))))), ProductionRule(BnfLiteral("sum_sub", NONTERM), SeqConstruct(List(GroupConstruct(List(BnfLiteral("product_div", NONTERM), RepeatConstruct(List(GroupConstruct(List(GroupConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("+", TERM))), GroupConstruct(List(BnfLiteral("-", TERM))))))), BnfLiteral("product_div", NONTERM)))))))))), ProductionRule(BnfLiteral("product_div", NONTERM), SeqConstruct(List(GroupConstruct(List(OptionalConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("+", TERM))), GroupConstruct(List(BnfLiteral("-", TERM))))))), BnfLiteral("term", NONTERM), RepeatConstruct(List(GroupConstruct(List(GroupConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("*", TERM))), GroupConstruct(List(BnfLiteral("/", TERM))))))), BnfLiteral("number", NONTERM)))))))))), ProductionRule(BnfLiteral("term", NONTERM), SeqConstruct(List(UnionConstruct(List(GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))), GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM)))))))), ProductionRule(BnfLiteral("number", NTREGEX), BnfLiteral("""(\+|\-)?[0-9]+(\.[0-9]+)?""", REGEXTERM)))

  it should "determine if a convergent expression grammar is convergent" in {
    val grw = new GrammarRewriter(convergentGrammar)
    val divergentNTs: List[BnFGrammarIR] = grw.grammarConvergenceChecker()
    if divergentNTs.isEmpty then logger.info("The grammar is convergent")
    else
      logger.error("The grammar is divergent")
      divergentNTs.foreach {
        nt => logger.error(s"Divergent NT: ${nt.uuid} -> ${nt.toString}")
      }
    divergentNTs shouldBe List()
  }

  it should "determine if a divergent expression grammar is divergent" in {
    val grw = new GrammarRewriter(divergentGrammar)
    val divergentNTs: List[BnFGrammarIR] = grw.grammarConvergenceChecker()
    if divergentNTs.isEmpty then logger.info("The grammar is convergent")
    else
      logger.error("The grammar is divergent")
      divergentNTs.foreach {
        nt => logger.error(s"Divergent NT: ${nt.uuid} -> ${nt.toString}")
      }
    divergentNTs.toString() shouldBe List(
      UnionConstruct(List(
        GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))),
        GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM))))
      ),
      UnionConstruct(List(
        GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))),
        GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM))))
      ),
      UnionConstruct(List(
        GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))),
        GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM))))
      ),
      UnionConstruct(List(
        GroupConstruct(List(BnfLiteral("sum_sub", NONTERM))),
        GroupConstruct(List(BnfLiteral("(", TERM), BnfLiteral("expression", NONTERM), BnfLiteral(")", TERM)))))
    ).toString()
  }

}
