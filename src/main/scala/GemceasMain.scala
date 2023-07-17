import Compiler.{BnfGrammarCompiler, LoadGrammarFile}
import Utilz.CreateLogger

import java.io.{File, FileNotFoundException}
import java.net.URL
import java.nio.file.Paths
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

/*******************************************************************************
 * Copyright (c) 7/16/23, 4:39 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

object GemceasMain:
  private lazy val logger = CreateLogger(classOf[GemceasMain.type])

  @main def runMain_GemceasMain(): Unit =
    val grammarFilePath = "/Grammars/ArithmeticExpressions.bnf"
    val srcGrammar:String = LoadGrammarFile(grammarFilePath) match
      case Failure(exception) =>
        logger.error(s"Failed to load a grammar because of $exception")
        "ERROR"
      case Success(code) => code
    logger.info(srcGrammar)
    val ast = BnfGrammarCompiler(srcGrammar)
    logger.info(ast.toString)