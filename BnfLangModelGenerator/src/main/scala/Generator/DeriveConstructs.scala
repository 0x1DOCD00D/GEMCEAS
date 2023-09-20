/*
 Copyright (c) 8/1/23, 1:46 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, BnfLiteral, GroupConstruct, IrError, IrLiteral, OptionalConstruct, ProductionRule, ProgramEntity, PrologFactsBuilder, RepeatConstruct, SeqConstruct, UnionConstruct}
import Utilz.{CreateLogger, PrologTemplate}
import org.slf4j.Logger

trait DeriveConstructs:
  val logger: Logger = CreateLogger(classOf[DeriveConstructs])

  val funcContainedConstructs: BnFGrammarIR => List[BnFGrammarIR] =
    {
      case container1: BnFGrammarIRContainer => container1.bnfObjects.flatMap(construct => deriveElement(construct))
      case _ => List()
    }

  def deriveElement(e: BnFGrammarIR, limit: Boolean = false): List[BnFGrammarIR] =
    e match
      case ir @ OptionalConstruct(bnfObjects) => OptionalConstructProcessor(ir, limit)
      case ir @ RepeatConstruct(bnfObjects) => RepeatConstructProcessor(ir, limit)
      case ir @ GroupConstruct(bnfObjects) => GroupConstructProcessor(ir)
      case ir @ SeqConstruct(bnfObjects) => SeqConstructProcessor(ir)
      case ir @ UnionConstruct(bnfObjects) => UnionConstructProcessor(ir, limit)
      case literal if literal.isInstanceOf[BnfLiteral] =>
        val res = LiteralProcessor(literal.asInstanceOf[BnfLiteral])
        if res.isEmpty then
          logger.error(s"IR structure contains a wrong element $literal.")
          List()
        else res
      case doneWithAlready if doneWithAlready.isInstanceOf[ProgramEntity] => List(doneWithAlready)
      case err =>
        logger.error(s"IR structure contains a wrong element $err.")
        List()
  end deriveElement
