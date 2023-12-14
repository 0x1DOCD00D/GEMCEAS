import Compiler.{BnfGrammarCompiler, LoadGrammarFile}
import LexerParser.*
import Utilz.CreateLogger

import java.io.{File, FileNotFoundException}
import java.net.URL
import java.nio.file.Paths
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

/*******************************************************************************
 * Copyright (c) 7/16/23, 4:39 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

object GemceasMain:
  private lazy val logger = CreateLogger(classOf[GemceasMain.type])
  /*
  expression ::= sum;

  sum ::= product {"+" product};

  product ::= ["+"|"-"] term {"*" term};

  term ::= number | "(" expression ")";

  <number> ::= "~>(\+|\-)?[0-9]+(\.[0-9]+)?<~";
  * */

  val result: MainRule =
    MainRule(List(
//      expression ::= sum;
      Rule(Nonterminal("expression"),RuleCollection(List(RuleLiteral(Nonterminal("sum"))))),
//      sum ::= product {"+" product};
      Rule(Nonterminal("sum"),
        RuleCollection(List(RuleLiteral(Nonterminal("product")),
          RuleCollection(List(
            RuleRep(
              RuleCollection(
                List(RuleLiteral(Terminal("+")), RuleCollection(List(RuleLiteral(Nonterminal("product")))))
              )
            )
          ))))
      ),
//      product ::= ["+"|"-"] term {"*" term};
      Rule(Nonterminal("product"),
        RuleCollection(List(
          RuleOpt(
            RuleCollection(List(RuleLiteral(Terminal("+")),
            RuleCollection(List(RuleOr(RuleCollection(List(RuleLiteral(Terminal("-")))))))))
          ),
          RuleCollection(List(RuleLiteral(Nonterminal("term")),
            RuleCollection(List(
              RuleRep(RuleCollection(List(RuleLiteral(Terminal("*")), RuleCollection(List(RuleLiteral(Nonterminal("term")))))))
            ))
          ))
        ))),
      Rule(Nonterminal("term"),RuleCollection(List(RuleLiteral(Nonterminal("number")),
        RuleCollection(List(
          RuleOr(
            RuleCollection(List(RuleLiteral(Terminal("(")),
            RuleCollection(List(RuleLiteral(Nonterminal("expression")), RuleCollection(List(RuleLiteral(Terminal(")")))))))))
        )))))
    ))

  @main def runMain_GemceasMain(): Unit =
    object A:
      object B:
        val xx = 2

    import com.github.dwickern.macros.NameOf.*
    val interestingVariable: (String, Int) = nameOf(interestingVariable) -> nameOf(interestingVariable).length
    logger.info(interestingVariable.toString())
    logger.info(qualifiedNameOfType[A.B.type])

    val `This is a somewhat. unusual var name. yeah!`: (String, Int) = nameOf(`This is a somewhat. unusual var name. yeah!`) -> nameOf(`This is a somewhat. unusual var name. yeah!`).length
    logger.info(`This is a somewhat. unusual var name. yeah!`.toString())
    val grammarFilePath = "/Grammars/ArithmeticExpressions.bnf"
    logger.info(nameOf(grammarFilePath))
    val srcGrammar:String = LoadGrammarFile(grammarFilePath)
    if srcGrammar.isEmpty then logger.error("Failed to load a grammar, terminating Gemceas.")
    else logger.info(srcGrammar)
//    logger.info(List.fill(0)("a").flatten.mkString)
/*    val ast = BnfGrammarCompiler(srcGrammar)
    logger.info(ast)*/