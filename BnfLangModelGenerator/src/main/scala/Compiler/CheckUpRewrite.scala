package Compiler

trait CheckUpRewrite:
  def isRewriteCompleted: Boolean
  def formListOfBnFGrammarElements: List[BnFGrammarIR]
