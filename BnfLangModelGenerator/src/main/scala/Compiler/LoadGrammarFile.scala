/*******************************************************************************
 * Copyright (c) 7/16/23, 1:25 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package Compiler

import Utilz.CreateLogger

import scala.util.{Failure, Success, Try, Using}
import scala.io.Source
import java.io.FileNotFoundException

object LoadGrammarFile:
  private val logger = CreateLogger(classOf[LoadGrammarFile.type])
  def apply(grammarFilePath: String): String =
    Using(Source.fromURL(getClass.getResource(grammarFilePath))) {
      reader =>
        reader.getLines().mkString("\n")
    } match
      case Failure(exception) =>
        logger.error(s"Cannot obtain a grammar string from $grammarFilePath for reason $exception")
        ""
      case Success(grammar) => grammar
