/*******************************************************************************
 * Copyright (c) 7/12/23, 11:25 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package LexerParser

import java.util.regex.Pattern
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import GrammarCompilationError.*
import Utilz.ConfigDb.debugLexerTokens
import Utilz.CreateLogger

object BnfGrammarLexer extends JavaTokenParsers:
  private lazy val logger = CreateLogger(classOf[BnfGrammarLexer.type])
  def apply(srcGrammar: String): Either[GrammarCompilationError,Seq[LexerToken]] = parse(BnfTokens, srcGrammar) match {
    case Failure(msg, next) => Left(BnfLexerError(Location(next.pos.line, next.pos.column), msg))
    case Error(msg, next) => Left(BnfLexerError(Location(next.pos.line, next.pos.column), msg))
    case Success(extractedTokens, _) => Right(extractedTokens)
  }

  def BnfTokens: Parser[List[LexerToken]] = phrase(rep1(isDefinedAs | verticalBar | bra | ket |
        curlybra | curlyket | nonterminal | nonterminalRegex | terminal | regex_string | endOfRule |
        leftParen | rightParen | singleLineComment | multiLineComment))
    ^^ { allCapturedTokens => allCapturedTokens.filterNot(_ == COMMENT()) }

  def isDefinedAs:Parser[ISDEFINEDAS] = positioned {
    "::=" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed ::=")
      ISDEFINEDAS())
  }

  def endOfRule: Parser[SEMICOLON] = positioned {
    ";" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed ;")
      SEMICOLON())
  }

  def verticalBar: Parser[VERTICALBAR] = positioned {
    "|" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed |")
      VERTICALBAR())
  }

  def bra: Parser[BRA] = positioned {
    "[" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed [")
      BRA())
  }

  def ket: Parser[KET] = positioned {
    "]" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed ]")
      KET())
  }

  def curlybra: Parser[CURLYBRA] = positioned {
    "{" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed {")
      CURLYBRA())
  }

  def curlyket: Parser[CURLYKET] = positioned {
    "}" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed }")
      CURLYKET())
  }

  def leftParen: Parser[LEFTPAREN] = positioned {
    "(" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed (")
      LEFTPAREN())
  }

  def rightParen: Parser[RIGHTPAREN] = positioned {
    ")" ^^ (_ =>
      if debugLexerTokens then logger.info("Lexed )")
      RIGHTPAREN())
  }

  def nonterminal: Parser[Nonterminal] = positioned {
    "[_a-zA-Z][-#$\\.:_a-zA-Z0-9]*".r ^^ { id =>
      if debugLexerTokens then logger.info(s"Lexed nt: $id")
      Nonterminal(id) }
  }

  def nonterminalRegex: Parser[NonterminalRegex] = positioned {
    "<[_a-zA-Z][-#$\\.:_a-zA-Z0-9]*>".r ^^ { nt =>
      if debugLexerTokens then logger.info(s"Lexed nt regex: $nt")
      NonterminalRegex(removeFirstAndLastDoubleQuotes(nt)) }
  }

  val removeFirstAndLastDoubleQuotes: String => String = (s: String) => if s.charAt(0) == '\"' && s.charAt(s.length-1) == '\"' then s.substring(1, s.length - 1) else s
  val removeWigglyArrows: String => String = (s: String) => s.substring(2, s.length - 2)

  def terminal: Parser[Terminal] =
    positioned{
      stringLiteral ^^ { strContent =>
        if debugLexerTokens then logger.info(s"Lexed terminal: $strContent")
        Terminal(removeFirstAndLastDoubleQuotes(strContent))
      }
    }

  def regex_string: Parser[RegexString] =
    positioned {
      "\"~>[^\n]+<~\"".r ^^ { strContent =>
        if debugLexerTokens then logger.info(s"Lexed regex: ${removeWigglyArrows(removeFirstAndLastDoubleQuotes(strContent))}")
        RegexString(removeWigglyArrows(removeFirstAndLastDoubleQuotes(strContent)))
      }
    }


  def singleLineComment: Parser[COMMENT] = """(//)(.*[\n\r])""".r ^^ { comment =>
    if debugLexerTokens then logger.info(s"Parsed comment: $comment")
    COMMENT()
  }

  def multiLineComment: Parser[COMMENT] =
    """(/\\*)(.|\n|\r)*?(\\*/)""".r ^^ { comment =>
      if debugLexerTokens then logger.info(s"Parsed multiline comment: $comment")
      COMMENT()
    }
