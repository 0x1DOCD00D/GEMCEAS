package Compiler

trait CheckUpRewrite:
  this: PrologFact| RepeatPrologFact =>
  def isRewriteCompleted(): Boolean =
    val bnfElements: List[BnFGrammarIR] = this match {
      case fact: PrologFact => fact.mapParams2GrammarElements.flatMap(_._2)
      case rfact: RepeatPrologFact => rfact.bnfObjects
    }

    bnfElements.forall {
    case fact: PrologFact => fact.isRewriteCompleted()
    case rfact: RepeatPrologFact => rfact.isRewriteCompleted()
    case pe: ProgramEntity => true
    case err => false
  }
  def formListOfBnFGrammarElements: List[BnFGrammarIR]
