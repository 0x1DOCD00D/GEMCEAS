/*******************************************************************************
 * Copyright (c) 7/16/23, 11:47 AM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
 *  
 * Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 ******************************************************************************/

package Utilz

import Utilz.ConfigReader.getConfigEntry
import Utilz.{Constants, CreateLogger}
import Utilz.Constants.*
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.Logger

import java.io.File
import java.lang.reflect.Field
import java.nio.charset.StandardCharsets
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
        maxRepeatConstruct = 10
        prologVerification = true
    }
    CodeConstraints {
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
  import com.github.dwickern.macros.NameOf.*
  val logger: Logger = CreateLogger(classOf[ConfigDb.type])
  val `Gemceas.seed`: Long = getConfigEntry(nameOf(`Gemceas.seed`), -1L)
  val `Gemceas.outputDirectory`: String = {
    val defDir = new java.io.File(".").getCanonicalPath
    logger.info(s"Default output directory: $defDir")
    val dir: String = getConfigEntry(nameOf(`Gemceas.outputDirectory`), defDir)
    val ref = new File(dir)
    if ref.exists() && ref.isDirectory then
      logger.info(s"Using output directory: $dir")
      if dir.endsWith(File.separator) then dir else dir + File.separator
    else
      logger.error(s"Output directory $dir does not exist or is not a directory, using current directory instead: $defDir")
      defDir
  }
  val `Gemceas.Generator.debugProgramGeneration`: Boolean = getConfigEntry(nameOf(`Gemceas.Generator.debugProgramGeneration`), true)
  val `Gemceas.Generator.debugProductionRules`: Boolean = getConfigEntry(nameOf(`Gemceas.Generator.debugProductionRules`), true)
  val `Gemceas.Generator.debugLexerTokens`: Boolean = getConfigEntry(nameOf(`Gemceas.Generator.debugLexerTokens`), true)
  val `Gemceas.Generator.grammarMaxDepthRewriting`: Int = getConfigEntry(nameOf(`Gemceas.Generator.grammarMaxDepthRewriting`), 10)
  val `Gemceas.Generator.maxDepthMultiplier`: Int = getConfigEntry(nameOf(`Gemceas.Generator.maxDepthMultiplier`), 10)
  val grammarMaxDepthRewritingWithError: Int = `Gemceas.Generator.grammarMaxDepthRewriting` * `Gemceas.Generator.maxDepthMultiplier`
  val `Gemceas.Generator.maxRepeatConstruct`: Int = getConfigEntry(nameOf(`Gemceas.Generator.maxRepeatConstruct`), 10)
  val `Gemceas.Generator.prologVerification`: Boolean = getConfigEntry(nameOf(`Gemceas.Generator.prologVerification`), true)
  val `Gemceas.CodeConstraints.stringLengthMin`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.stringLengthMin`), 3)
  val `Gemceas.CodeConstraints.stringLengthMax`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.stringLengthMax`), 10)
  val `Gemceas.CodeConstraints.intLengthMin`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.intLengthMin`), 1)
  val `Gemceas.CodeConstraints.intLengthMax`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.intLengthMax`), 3)
  val `Gemceas.CodeConstraints.floatLengthMin`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.floatLengthMin`), 1)
  val `Gemceas.CodeConstraints.floatLengthMax`: Int = getConfigEntry(nameOf(`Gemceas.CodeConstraints.floatLengthMax`), 2)

  private val checkTypeOfField: Field => Boolean = field =>
    field.getType == classOf[Boolean] ||
    field.getType == classOf[Int] ||
    field.getType == classOf[Double] ||
    field.getType == classOf[Float] ||
    field.getType == classOf[Long] ||
    field.getType == classOf[String]


  private def getFields: Map[String, String] =
    this.getClass.getDeclaredFields.filter(checkTypeOfField).map(field => field.getName.replaceAll("""\$u002E""", ".") -> field.get(this).toString).toMap[String, String]

  def apply(): Map[String, String] = getFields