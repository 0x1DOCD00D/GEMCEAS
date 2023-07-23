/*
 Copyright (c) 7/23/23, 10:51 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

trait BnFGrammarIR
type BnFGrammarIrMap = Map[String, List[BnFGrammarIR]]

trait OptionalConstruct extends BnFGrammarIR
trait RepeatConstruct extends BnFGrammarIR
trait GroupConstruct extends BnFGrammarIR
trait UnionConstruct extends BnFGrammarIR
trait Literal extends BnFGrammarIR
trait RegExSpec extends Literal
trait PlainText extends Literal

case class IrError(err: String) extends BnFGrammarIR
