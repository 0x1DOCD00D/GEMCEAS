package Compiler

trait CheckUpRewrite:
  def isRewriteCompleted(bnfElements: List[BnFGrammarIR]): Boolean = bnfElements.forall {
    case fact: PrologFact => fact.isRewriteCompleted(fact.mapParams2GrammarElements.flatMap(_._2))
    case rfact: RepeatPrologFact => rfact.isRewriteCompleted(rfact.bnfObjects)
    case pe: ProgramEntity => true
    case err => false
  }
  def formListOfBnFGrammarElements: List[BnFGrammarIR]
