/*******************************************************************************
 * Copyright (c) 7/16/23, 11:47 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *  
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package Utilz

import Utilz.ConfigReader.getConfigEntry
import Utilz.{CreateLogger, Constants}
import Utilz.Constants.*
import org.slf4j.Logger

import java.io.File
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Random, Success, Try}
import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector

/*
Gemceas {
    seed = 10
    outputDirectory = "/Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/gemceas/outputs"
    Generator {
        debugProgramGeneration = true
        debugProductionRules = true
        debugLexerTokens = true
        grammarMaxDepthRewriting = 10
        maxDepthMultiplier = 10
        prologVerification = true
    }
    CodeStructure {
        maxLineCharacters = 80
        stringLengthMin = 3
        stringLengthMax = 10
        intLengthMin = 1
        intLengthMax = 3
        floatLengthMin = 1
        floatLengthMax = 2
    }
}
* */


object ConfigDb:
  val logger: Logger = CreateLogger(classOf[ConfigDb.type])
  val debugProgramGeneration: Boolean = getConfigEntry(Constants.configGenerator, DEBUGPROGRAMGENERATION, DEBUGPROGRAMGENERATIONDEFAULT)
  val debugProductionRules: Boolean = getConfigEntry(Constants.configGenerator, DEBUGPRODUCTIONRULES, DEBUGLEXERTOKENSDEFAULT)
  val debugLexerTokens: Boolean = getConfigEntry(Constants.configGenerator, DEBUGLEXERTOKENS, DEBUGLEXERTOKENSDEFAULT)
  val grammarMaxDepthRewriting: Int = getConfigEntry(Constants.configGenerator, GRAMMARMAXDEPTHREWRITING, GRAMMARMAXDEPTHREWRITINGDEFAULT)
  val maxDepthMultiplier: Int = getConfigEntry(Constants.configGenerator, MAXDEPTHMULTIPLIER, MAXDEPTHMULTIPLIERDEFAULT)
  val maxRepeatConstruct: Int = getConfigEntry(Constants.configGenerator, MAXREPEATCONSTRUCT, MAXREPEATCONSTRUCTDEFAULT)
  val grammarMaxDepthRewritingWithError: Int = grammarMaxDepthRewriting * maxDepthMultiplier
  val outputDirectory: String = {
    val defDir = new java.io.File(".").getCanonicalPath
    logger.info(s"Default output directory: $defDir")
    val dir: String = getConfigEntry(Constants.globalConfig, Constants.OUTPUTDIRECTORY, defDir)
    val ref = new File(dir)
    if ref.exists() && ref.isDirectory then
      logger.info(s"Using output directory: $dir")
      if dir.endsWith(File.separator) then dir else dir + File.separator
    else
      logger.error(s"Output directory $dir does not exist or is not a directory, using current directory instead: $defDir")
      defDir
  }

  def getFields: Map[String, Double] = this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Double]).map(field => field.getName -> field.get(this).asInstanceOf[Double]).toMap[String, Double] ++ this.getClass.getDeclaredFields.filter(field => field.getType == classOf[Int]).map(field => field.getName -> field.get(this).toString.toDouble).toMap[String, Double]