/*******************************************************************************
 * Copyright (c) 7/12/23, 11:25 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package LexerParser

import java.util.regex.Pattern
import scala.util.parsing.combinator.JavaTokenParsers
import GrammarCompilationError.*
import BnfGrammarAST.*
import Utilz.ConfigDb.debugLexerTokens
import Utilz.CreateLogger

object BnfGrammarLexer extends JavaTokenParsers:
  private lazy val logger = CreateLogger(classOf[BnfGrammarLexer.type])
  def apply(srcGrammar: String): GrammarCompilationError | Seq[BnfGrammarAST] = parse(BnfTokens, srcGrammar) match {
    case Failure(msg, next) => BnfLexerError(Location(next.pos.line, next.pos.column), msg)
    case Error(msg, next) => BnfLexerError(Location(next.pos.line, next.pos.column), msg)
    case Success(result, next) => result.toSeq
  }

  def BnfTokens: Parser[List[BnfGrammarAST]] = phrase(rep1(isDefinedAs | verticalBar | bra | ket |
        curlybra | curlyket | less | greater | nonterminal | terminal | regex_string | singleLineComment | multiLineComment))
    ^^ { allCapturedTokens => allCapturedTokens.filterNot(_ == COMMENT) }

  def isDefinedAs:Parser[ISDEFINEDAS] = positioned {
    "::=" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed ::=")
      ISDEFINEDAS())
  }

  def verticalBar: Parser[VERTICALBAR] = positioned {
    "|" ^^ (_ => VERTICALBAR())
  }

  def bra: Parser[BRA] = positioned {
    "[" ^^ (_ => BRA())
  }

  def ket: Parser[KET] = positioned {
    "]" ^^ (_ => KET())
  }

  def curlybra = positioned {
    "{" ^^ (_ => CURLYBRA())
  }

  def curlyket = positioned {
    "}" ^^ (_ => CURLYKET())
  }

  def less = positioned {
    "<" ^^ (_ => LESS())
  }

  def greater = positioned {
    ">" ^^ (_ => GREATER())
  }

  def nonterminal: Parser[Nonterminal] = positioned {
    "[a-zA-Z][-#$\\.:_a-zA-Z0-9]*".r ^^ { id =>
      if debugLexerTokens then logger.info(s"Lexed nt: $id")
      Nonterminal(id) }
  }

  def nonterminalRegex: Parser[NonterminalRegex] = positioned {
    less ~> nonterminal <~ greater ^^ { nt =>
      if debugLexerTokens then logger.info(s"Lexed nt regex: $nt")
      NonterminalRegex(nt.id) }
  }

  val removeDoubleQuotes = (s: String) => s.substring(1, s.length - 1)
  val removeWigglyArrows = (s: String) => s.substring(2, s.length - 2)

  def terminal: Parser[Terminal] =
    positioned{
      stringLiteral ^^ { strContent =>
        if debugLexerTokens then logger.info(s"Lexed terminal: $strContent")
        Terminal(removeDoubleQuotes(strContent))
      }
    }

  def regex_string: Parser[RegexString] =
    positioned {
      "\"~>[^\n]+<~\"".r ^^ { strContent =>
        if debugLexerTokens then logger.info(s"Lexed regex: ${removeWigglyArrows(removeDoubleQuotes(strContent))}")
        RegexString(removeWigglyArrows(removeDoubleQuotes(strContent)))
      }
    }

  def doubleSlash = positioned("//" ^^^ DOUBLESLASH)

  def openComment = positioned("/*" ^^^ SLASHSTAROPEN)

  def singleLineComment = doubleSlash ~ rep(not("\n") ~ ".".r) ^^^ COMMENT
  def multiLineComment = openComment ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ COMMENT
