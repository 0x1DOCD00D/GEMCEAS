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
import BnfGrammarAST.*
import LexerParser.BnfGrammarLexer.{bra, isDefinedAs, nonterminal, terminal}
import LexerParser.GrammarCompilationError.BnfLexerError
import Utilz.ConfigDb.debugLexerTokens
import Utilz.CreateLogger

/*
   mainRule           ::= rule | rule mainRule
   rule               ::= rule-name_regex "::=" rhs
   <rule-name>        ::= "[<a-zA-Z][-#>$\.:_a-zA-Z0-9]*"
   rhs                ::= ruleContent | ruleContent "|" rhs | "[" rhs "]" | "{" rhs "}"
   ruleContent        ::= term | term ruleContent
   term               ::= <literal> | <rule-name>
   <literal>          ::= "\".*\"" | "\'\.*\'"
 */

object BnfGrammarParser extends Parsers with PackratParsers with DebugParserUtil:
  val logger = CreateLogger(classOf[BnfGrammarParser.type])
  override type Elem = BnfGrammarAST

  case class BnfGrammarTokenReader(bnfTokens: Seq[BnfGrammarAST]) extends Reader[BnfGrammarAST]:
    if debugLexerTokens then logger.info(s"Tokens\n ${bnfTokens.mkString(", ")}")
    override def first: BnfGrammarAST = bnfTokens.headOption match
      case Some(value) => if debugLexerTokens then logger.info(s"Token: $value}")
        value
      case None => NOTOKEN

    override def rest: Reader[BnfGrammarAST] = BnfGrammarTokenReader(bnfTokens.tail)

    override def pos: Position = bnfTokens.headOption.map(_.pos).getOrElse(NoToken)

    override def atEnd: Boolean = bnfTokens.isEmpty
  end BnfGrammarTokenReader

  def apply(bnfTokens: Seq[BnfGrammarAST] | GrammarCompilationError): GrammarCompilationError | BnfGrammarAST =
    import GrammarCompilationError.{BnfParserError, Location}
    if bnfTokens.isInstanceOf[GrammarCompilationError] then
      logger.error(s"Error when lexing, parsing is abandoned...")
      bnfTokens.asInstanceOf[GrammarCompilationError]
    else
      val tokenReader = BnfGrammarTokenReader(bnfTokens.asInstanceOf[Seq[BnfGrammarAST]])
      mainFuleProcessor(tokenReader) match {
        case NoSuccess(msg, next) ⇒ BnfParserError(Location(next.pos.line, next.pos.column), msg)
        case Success(ast, _) ⇒ ast
      }
  end apply

  lazy val mainFuleProcessor: Parser[MainRule] = positioned {
    val simpleRule: Parser[MainRule] = rule ^^ {
      case rl => MainRule(rl, None)
    }
    val complexRule: Parser[MainRule] = rule ~ mainFuleProcessor ^^ {
      case rl ~ mr => MainRule(rl, Some(mr))
    }

    show(simpleRule)("simpleRule") | show(complexRule)("complexRule")
  }

  lazy val rule: PackratParser[Rule] = positioned {
    val basicRuleName: Parser[Rule] = non_terminal ~ defdAs ~ rhs ^^ {
      case nt ~ _ ~ exp => Rule(nt, exp)
    }
    val regexBasedRule: Parser[Rule] = less ~ non_terminal ~ gt ~ defdAs ~ rhs ^^ {
      case _ ~ nt ~ _ ~ _ ~ exp => Rule(NonterminalRegex(nt.id), exp)
    }
    show(basicRuleName)("basicRuleName") | show(regexBasedRule)("regexBasedRule")
  }


  private def rhs: PackratParser[RHS] = positioned {
    val justRuleContent: Parser[RHS] = ruleContent ^^ {
      case rc => RHS(Some(rc), None)
    }

    val ruleContentOrRHS: Parser[RHS] = ruleContent ~ Or ~ rhs ^^ {
      case rc ~ _ ~ exp => RHS(Some(rc), Some(exp))
    }

    val optionalRHS: Parser[RHS] = Bra ~ rhs ~ Ket ^^ {
      case _ ~ exp ~ _ => RHS(None, Some(exp), BnfAttributeFlag.OPTIONAL)
    }

    val repRHS: Parser[RHS] = CurlyBra ~ rhs ~ CurlyKet ^^ {
      case _ ~ exp ~ _ => RHS(None, Some(exp), BnfAttributeFlag.REPETITIVE)
    }
    show(justRuleContent)("justRuleContent") | show(ruleContentOrRHS)("ruleContentOrRHS") | show(optionalRHS)("optionalRHS") | show(repRHS)("repRHS")
  }

  private def ruleContent: Parser[RuleContent] = positioned {
    val justTerm: Parser[RuleContent] = aTerminal ^^ {
      case term => RuleContent(term, None)
    }
    show(justTerm)("justTerm")
  }

  private def non_terminal: Parser[Nonterminal] = positioned {
    accept("nonterminal", { case id@Nonterminal(name) => id })
  }

  private def aTerminal: Parser[Terminal] = positioned {
    accept("terminal", { case id@Terminal(value) => id })
  }

  private def defdAs: PackratParser[ISDEFINEDAS] = positioned {
    accept("::= separator", { case ds@ISDEFINEDAS() => ds })
  }

  private def less: PackratParser[LESS] = positioned {
    accept("< rule name", { case ls@LESS() => ls })
  }

  private def gt: PackratParser[GREATER] = positioned {
    accept("rule name >", { case g8er@GREATER() => g8er })
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
    accept("opening {", { case bra@CURLYBRA() => bra })
  }

  private def CurlyKet: PackratParser[CURLYKET] = positioned {
    accept("closing }", { case ket@CURLYKET() => ket })
  }

end BnfGrammarParser