/*******************************************************************************
 * Copyright (c) 7/16/23, 11:21 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package LexerParser

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}
import Utilz.ConfigDb.debugProductionRules

trait DebugParserUtil:
  self: PackratParsers =>
    def show[T](p: => PackratParser[T])(name: String): Parser[T] =
      if (!debugProductionRules) p else log(p)(name)
