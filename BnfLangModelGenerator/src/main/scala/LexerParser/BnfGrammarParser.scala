/*******************************************************************************
 * Copyright (c) 7/13/23, 12:09 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package LexerParser

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}
import LexerParser.BnfGrammarLexer.*
import LexerParser.GrammarCompilationError.BnfLexerError
import Utilz.ConfigDb.{debugLexerTokens, debugProductionRules}
import Utilz.CreateLogger
import org.slf4j.Logger

/*
 mainRule             ::= {rule ";"}
 rule                 ::=  (non_terminal | non_terminal_regex) "::=" topRhs
 topRhs               ::= rhs {topRhs}
 rhs                  ::= (literal | non_terminal | non_terminal_regex) | "|" topRhs | "[" topRhs "]" | "{" topRhs "}" | "(" topRhs ")"
 non_terminal         ::= stringLiteral
 <non_terminal_regex> ::= stringLiteral
 literal              ::= doubleQuotedString
 */

object BnfGrammarParser extends Parsers with PackratParsers with DebugParserUtil:
  val logger: Logger = CreateLogger(classOf[BnfGrammarParser.type])
  override type Elem = LexerToken

  case class BnfGrammarTokenReader(bnfTokens: Seq[LexerToken]) extends Reader[LexerToken]:
    if debugLexerTokens then logger.info(s"Tokens:\n${bnfTokens.mkString(", ")}")
    override def first: LexerToken = bnfTokens.headOption match
      case Some(value) => if debugLexerTokens then logger.info(s"Token: $value}")
        value
      case None => UNKNOWNTOKEN()

    override def rest: Reader[LexerToken] = BnfGrammarTokenReader(bnfTokens.tail)

    override def pos: Position = bnfTokens.headOption.map(_.pos).getOrElse(new Position{
      override def line: Int = -1

      override def column: Int = -1

      override protected def lineContents: String = "Lexing failed."
    })

    override def atEnd: Boolean = bnfTokens.isEmpty
  end BnfGrammarTokenReader

  def apply(bnfTokens: Seq[LexerToken]): Either[GrammarCompilationError, BnfGrammarAST] =
    import GrammarCompilationError.{BnfParserError, Location}
    if bnfTokens.isEmpty then
      logger.error(s"Error when lexing, parsing is abandoned...")
      Left(BnfLexerError(Location(0,0), "no tokens present"))
    else
      val tokenReader = new PackratReader(BnfGrammarTokenReader(bnfTokens))
      mainFuleProcessor(tokenReader) match {
        case NoSuccess(msg, next) => Left(BnfParserError(Location(next.pos.line, next.pos.column), msg))
        case Failure(msg, next) => Left(BnfParserError(Location(next.pos.line, next.pos.column), msg))
        case Error(msg, next) => Left(BnfParserError(Location(next.pos.line, next.pos.column), msg))
        case Success(ast, _) => Right(ast)
      }
  end apply

//  mainRule             ::= {rule ";"}
  lazy val mainFuleProcessor: PackratParser[MainRule] = positioned {
    lazy val theMainRule = repsep(rule, semiColonEndsRule) ^^ (rl => MainRule(rl))

    show(theMainRule)("the mother of all rules")
  }

//rule                 ::=  (non_terminal | non_terminal_regex) "::=" topRhs
  lazy val rule: PackratParser[Rule] = positioned {
    lazy val aNtRgxRhs = aRegExp ^^ (lt => RuleLiteral(lt))
    val basicRule = non_terminal ~ defdAs ~ topRhs ^^ {
      case nt ~ _ ~ exp => Rule(nt, exp)
    }
    val regexRule = non_terminal_regex ~ defdAs ~ aNtRgxRhs ^^ {
      case nt ~ _ ~ exp => Rule(nt, exp)
    }
    show(basicRule)("basicRule") | show(regexRule)("regexRule")
  }

  lazy val topRhs: PackratParser[RuleCollection] = positioned {
    val topRule = rhs ~ rep(topRhs) ^^ {
      case nt ~ exp => RuleCollection(nt :: exp)
    }
    show(topRule)("top rule")
  }

  lazy val rhs: PackratParser[RuleContent] = positioned {
    lazy val litRule = ruleLiterals ^^ (rule => rule)

    lazy val optRule = Bra ~> topRhs <~ Ket ^^ (exp => RuleOpt(exp))

    lazy val orRule = Or ~> topRhs ^^ (exp => RuleOr(exp))

    lazy val repRule = CurlyBra ~> topRhs <~ CurlyKet ^^ (exp => RuleRep(exp))

    lazy val groupRule = lPar ~> topRhs <~ rPar ^^ (exp => RuleGroup(exp))

    show(litRule)("literal") | show(optRule)("[...]") | show(repRule)("{...}") | show(orRule)("...|...") | show(groupRule)("(...)")
  }

//ruleContent               ::= term {ruleContent};
  lazy val ruleLiterals: PackratParser[RuleLiteral] = positioned {
    lazy val aT = aTerminal ^^ (lt => RuleLiteral(lt))

    lazy val aNt = non_terminal ^^ (lt => RuleLiteral(lt))

    lazy val aNtRgx = non_terminal_regex ^^ (lt => RuleLiteral(lt))

    show(aT)("terminal") | show(aNt)("nonterminal") | show(aNtRgx)("nontermRegex")
  }

  private def non_terminal: PackratParser[Nonterminal] = positioned {
    accept("nonterminal", { case id@Nonterminal(name) => id })
  }

  private def non_terminal_regex: PackratParser[NonterminalRegex] = positioned {
    accept("nonterminal regex", { case id@NonterminalRegex(name) => id })
  }


  private def aTerminal: PackratParser[Terminal] = positioned {
    accept("terminal", { case Terminal(value) => Terminal(value) })
  }

  private def aRegExp: PackratParser[RegexString] = positioned {
    accept("regexp string", { case RegexString(value) => RegexString(value) })
  }

  private def defdAs: PackratParser[ISDEFINEDAS] = positioned {
    accept("::=", { case ds@ISDEFINEDAS() => ds })
  }

  private def Or: PackratParser[VERTICALBAR] = positioned {
    accept("this | that", { case or@VERTICALBAR() => or })
  }

  private def Bra: PackratParser[BRA] = positioned {
    accept("opening [", { case bra@BRA() => bra })
  }

  private def Ket: PackratParser[KET] = positioned {
    accept("closing ]", { case ket@KET() => ket })
  }

  private def CurlyBra: PackratParser[CURLYBRA] = positioned {
    accept("opening {", { case cbra@CURLYBRA() => cbra })
  }

  private def CurlyKet: PackratParser[CURLYKET] = positioned {
    accept("closing }", { case kt@CURLYKET() => kt })
  }

  private def lPar: PackratParser[LEFTPAREN] = positioned {
    accept("opening (", { case lp@LEFTPAREN() => lp })
  }

  private def rPar: PackratParser[RIGHTPAREN] = positioned {
    accept("closing )", { case rp@RIGHTPAREN() => rp })
  }

  private def semiColonEndsRule: PackratParser[SEMICOLON] = positioned {
    accept("closing rule with ;", { case sc@SEMICOLON() => sc })
  }

end BnfGrammarParser