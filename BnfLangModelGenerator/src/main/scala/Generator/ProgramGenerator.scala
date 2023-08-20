/*
 Copyright (c) 7/30/23, 2:26 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, BnfLiteral, GrammarRewriter, GroupConstruct, IrError, IrLiteral, LiteralType, OptionalConstruct, ProductionRule, ProgramEntity, RepeatConstruct, SeqConstruct, TerminationData, UnionConstruct}
import Generator.ProgramGenerator.logger
import Utilz.ConfigDb.{debugProgramGeneration, grammarUnrollDepthTermination}
import Utilz.CreateLogger

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class GeneratedProgramState(elements: List[BnFGrammarIR])

type GeneratedProgram = List[String]

class ProgramGenerator private (progGenState: GeneratedProgramState) extends DeriveConstructs:

  @tailrec
  private def derivedProgramInstance(prState: GeneratedProgramState = progGenState, level: Int = 0): GeneratedProgramState =
    if !prState.elements.forall(_.isInstanceOf[ProgramEntity]) then
      val accum = deriveProgram(List(), prState.elements, level > grammarUnrollDepthTermination)
      derivedProgramInstance(GeneratedProgramState(accum), level+1)
    else prState

  @tailrec
  private def deriveProgram(accumulatorProg: List[BnFGrammarIR], st: List[BnFGrammarIR], limit: Boolean): List[BnFGrammarIR] =
    st match
      case ::(head, next) => deriveProgram(accumulatorProg ::: deriveElement(head, limit), next, limit)
      case Nil => accumulatorProg
  end deriveProgram

  private def generateSourceCode(st: GeneratedProgramState): GeneratedProgram =
    if !st.elements.forall(_.isInstanceOf[ProgramEntity]) then
      logger.error(s"The generated program is not well structured.")
      List()
    else
      val code = st.elements.asInstanceOf[List[ProgramEntity]].flatMap(e => List(e.code))
      code
end ProgramGenerator

object ProgramGenerator:
  private val logger = CreateLogger(classOf[ProgramGenerator.type])
  private var _grammar: List[ProductionRule] = _
  private var reachabilityMap: Map[UUID, TerminationData] = Map()
  val expandNT: BnfLiteral => Option[ProductionRule] =
    (nt: BnfLiteral) =>
      if nt.literalType == LiteralType.TERM || nt.literalType == LiteralType.REGEXTERM then
        logger.error(s"BnF literal $nt cannot be used to define a rule.")
        None
      else _grammar.find(_.lhs.asInstanceOf[BnfLiteral].token == nt.token)

  def lookup(go: UUID): Option[TerminationData] = reachabilityMap.get(go)

  def apply(g: List[ProductionRule], startRuleId: BnfLiteral): Either[String,GeneratedProgram] =
    if g.length <= 0 then Left("Cannot derive a program using an empty grammar")
    else
      _grammar = g
      val gr = new GrammarRewriter(g)
      val divergentNTs: List[BnFGrammarIR] = gr.grammarConvergenceChecker()
      if divergentNTs.isEmpty then logger.info("The grammar is convergent")
      else
        logger.error("The grammar is divergent")
        divergentNTs.foreach {
          nt => logger.error(s"Divergent NT: ${nt.uuid} -> ${nt.toString}")
        }

      if debugProgramGeneration then logger.info(s"ProgramGenerator obtains the following reachability map: \n\n${reachabilityMap.mkString("\n\n")}")
      expandNT(startRuleId) match
        case Some(rl) =>
          val initState = GeneratedProgramState(List(rl.rhs))
          val gen = new ProgramGenerator(initState)
          val programObject: GeneratedProgramState = gen.derivedProgramInstance()
          val code: GeneratedProgram = gen.generateSourceCode(programObject)
          Right(code)
        case None =>
          val errStr = s"Cannot find a rule defined by nonterminal $startRuleId, choosing the first rule ${g.head} instead"
          logger.error(errStr)
          Left(errStr)

  @main def runMain_ProgramGenerator(): Unit =
    import Compiler.LiteralType.*
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */
    val grammar = List(
      ProductionRule(BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM))
          ))
        )
      ),
      ProductionRule(BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("+", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("-", TERM))
                    ))
                  ))
                ),
                BnfLiteral("product_div", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      product_div ::= ["+"|"-"] term {("*"|"/") term};
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("product_div", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              UnionConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("+", TERM))
                ),
                GroupConstruct(List(
                  BnfLiteral("-", TERM))
                ))
              ))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("*", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("/", TERM))
                    ))
                  ))
                ),
                BnfLiteral("term", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM))
            ))
          ))
        )
      ),
      ProductionRule(
        BnfLiteral("number", NTREGEX),
        BnfLiteral("""([+]|[-])?[0-9]+(\.[0-9]+)?""", REGEXTERM)
      )
    )

    val gen = ProgramGenerator(grammar, BnfLiteral("expression", NONTERM))
    logger.info(s"${gen.toString}")
end ProgramGenerator
