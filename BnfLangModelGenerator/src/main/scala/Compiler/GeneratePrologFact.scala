package Compiler

import Utilz.CheckIfStringIsNumber.*
import Utilz.Constants.{CloseKet, CommaSeparator, OpenBra}

trait GeneratePrologFact:
  this: PrologFact | RepeatPrologFact =>
  val listWrapper: (List[String], Boolean) => List[String] = (x: List[String], top: Boolean) =>
    if top then x
    else if x.isEmpty then List[String]().empty
    else if x.length == 1 then List(x.head)
    else
      val first = List(OpenBra + x.head)
      val last = List(x.reverse.head + CloseKet)
      first ::: x.slice(1, x.length - 2) ::: last

  val bnfElements: List[BnFGrammarIR] = this match {
    case fact: PrologFact => fact.mapParams2GrammarElements.flatMap(_._2)
    case rfact: RepeatPrologFact => rfact.bnfObjects
  }

  def generatePrologFact4KBLS(top: Boolean): String =
    val functorName: Option[String] = this match
      case fact: PrologFact => Some(fact.functorName)
      case _ => None
    val params: List[String] = bnfElements.foldLeft(List[String]()) {
      (acc, e) =>
        acc ::: listWrapper(List(e match
          case fact: PrologFact => fact.generatePrologFact4KBLS(false)
          case rfact: RepeatPrologFact => OpenBra + rfact.generatePrologFact4KBLS(false) + CloseKet
          case pe: ProgramEntity => if pe.code.IsNumber then pe.code else s""""${pe.code}"""".stripMargin
          case _ => e.toString), top)
    }
    if functorName.isEmpty then params.mkString(CommaSeparator.toString)
    else s"${functorName.get}(${params.mkString(CommaSeparator.toString)})"

  def formListOfBnFGrammarElements: List[BnFGrammarIR]