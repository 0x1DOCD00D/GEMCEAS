package Generator

import Compiler.{BnFGrammarIR, PrologFact, RepeatPrologFact}

case class GeneratedProgramState(elements: List[BnFGrammarIR]):
  def convertPrologFactsIntoBnFElements(): GeneratedProgramState =
    val newElements = elements.flatMap {
      case pf: PrologFact => pf.formListOfBnFGrammarElements
      case repeatPrologFact: RepeatPrologFact => repeatPrologFact.formListOfBnFGrammarElements
      case _ => List()
    }
    GeneratedProgramState(newElements)

type GeneratedProgram = List[String]
