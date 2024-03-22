package Generator

import Compiler.{BnFGrammarIR, BnfLiteral, PrologFact, RepeatPrologFact, GroupConstruct, SeqConstruct}
import Compiler.LiteralType.NONTERM
import Utilz.CreateLogger

object GelChecks:
  private val logger = CreateLogger(classOf[GelChecks.type])
  def isGelNt(gel: BnFGrammarIR, tokenValue: Option[String] = None): Boolean =
    gel match
      case BnfLiteral(token, NONTERM, _) => if tokenValue.isDefined then token == tokenValue.get.trim() else true
      case wrongGel => false

  def isGelTransitNode(gel: BnFGrammarIR): Boolean =
    gel match
      case GroupConstruct(_, _) => true
      case SeqConstruct(_, _) => true
      case PrologFact(_, _, _) => true
      case RepeatPrologFact(_, _) => true
      case wrongGel => false
