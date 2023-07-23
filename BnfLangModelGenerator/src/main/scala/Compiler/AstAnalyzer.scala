/*
 Copyright (c) 7/22/23, 7:01 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import LexerParser.{BnfGrammarAST, MainRule, PARSEFAILURE, Rule, RuleContent}

object AstAnalyzer:
  def apply(ast: BnfGrammarAST): Either[IrError, BnFGrammarIrMap] =
    def constructIr(lOr: List[Rule]): Either[IrError, BnFGrammarIrMap] =
      val ir =  lOr.map {
        rule => null

      }
      ???
    end constructIr

    ast match
      case MainRule(rules) => constructIr(rules)
      case PARSEFAILURE(err) => Left(IrError(err))
      case err => Left(IrError(s"Ast should have MainRule as the root, not ${err.toString}"))
  end apply


  @main def runMain_AstAnalyzer(): Unit =
    println(AstAnalyzer.getClass.getName)
