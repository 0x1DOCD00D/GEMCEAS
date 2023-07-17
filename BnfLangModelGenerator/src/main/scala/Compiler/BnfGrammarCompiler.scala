/*******************************************************************************
 * Copyright (c) 7/15/23, 8:01 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package Compiler

import LexerParser.{BnfGrammarAST, BnfGrammarLexer, BnfGrammarParser, GrammarCompilationError}
import Utilz.CreateLogger

object BnfGrammarCompiler:
  private lazy val logger = CreateLogger(classOf[BnfGrammarCompiler.type])

  def apply(srcGrammar: String): BnfGrammarAST =
    val parsed: Either[LexerParser.GrammarCompilationError, (LexerParser.GrammarCompilationError | Seq[LexerParser.BnfGrammarAST],
      LexerParser.GrammarCompilationError | LexerParser.BnfGrammarAST)] = for {
      lexTokens <- BnfGrammarLexer(srcGrammar) match
        case err: GrammarCompilationError => logger.error(s"Failed to lex the grammar: $err")
          Left(err)
        case lst => Right(lst)
      ast <- BnfGrammarParser(lexTokens) match
        case err: GrammarCompilationError => logger.error(s"Failed to parse the grammar: $err")
          Left(err)
        case lst => Right(lst)
    } yield (lexTokens: GrammarCompilationError | Seq[BnfGrammarAST], ast: GrammarCompilationError | BnfGrammarAST)
    parsed match
      case Left(_) => BnfGrammarAST.PARSEFAILURE
      case Right(ast) => ast._2 match
        case _:GrammarCompilationError => BnfGrammarAST.PARSEFAILURE
        case result:BnfGrammarAST => result