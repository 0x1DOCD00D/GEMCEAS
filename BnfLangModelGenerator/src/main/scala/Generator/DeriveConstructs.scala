/*
 Copyright (c) 8/1/23, 1:46 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Generator

import Compiler.{BnFGrammarIR, BnfLiteral, GroupConstruct, MetaVariable, OptionalConstruct, ProgramEntity, RepeatConstruct, SeqConstruct, UnionConstruct}
import Utilz.CreateLogger
import org.slf4j.Logger

trait DeriveConstructs:
  val logger: Logger = CreateLogger(classOf[DeriveConstructs])
  def deriveElement(e: BnFGrammarIR, limit: Boolean): List[BnFGrammarIR] =
    val replicatedGels = e match
        case ir @ OptionalConstruct(bnfObjects, _) => OptionalConstructProcessor(ir, limit)
        case ir @ RepeatConstruct(bnfObjects, _) => RepeatConstructProcessor(ir, limit)
        case ir @ GroupConstruct(bnfObjects, _) => GroupConstructProcessor(ir)
        case ir @ SeqConstruct(bnfObjects, _) => SeqConstructProcessor(ir)
        case ir @ UnionConstruct(bnfObjects, _) => UnionConstructProcessor(ir, limit)
        case mv if mv.isInstanceOf[MetaVariable] => List(mv)
        case literal if literal.isInstanceOf[BnfLiteral] =>
          val res = LiteralProcessor(literal.asInstanceOf[BnfLiteral])
          if res.isEmpty then
            logger.error(s"Literal IR structure contains incorrect $literal.")
            List()
          else res
        case doneWithAlready if doneWithAlready.isInstanceOf[ProgramEntity] => List(doneWithAlready)
        case err =>
          logger.error(s"IR structure contains the wrong element $err.")
          List()
    replicatedGels.map(_.replicaWithUniqueID)
  end deriveElement
