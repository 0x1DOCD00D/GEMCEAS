package Generator

import Compiler.{BnFGrammarIR, BnfLiteral}
import Compiler.LiteralType.{NONTERM}

object GelChecks:
  def isGelNt(gel: BnFGrammarIR, tokenValue: Option[String] = None): Boolean =
    gel match
      case BnfLiteral(token, NONTERM, _) =>
        if tokenValue.isDefined then token == tokenValue.get.trim() else true
      case _ => false
