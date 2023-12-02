/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */
package Utilz

import com.typesafe.config.{Config, ConfigFactory}

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.Failure

object Constants:
  private val config: Config = ConfigFactory.load()
  case class EnumeratedLoopParameters(ps: List[Double])
  case class FromToWithStepParameters(from: Double, to: Double, step: Double)

  final val Prolog_Template_Designator = "==>>"
  final val Prolog_Template_Assignment = "=:="
  final val OpenParen = '('
  final val CloseParen = ')'
  final val OpenBra = "["
  final val CloseKet = "]"
  final val CommaSeparator = ','

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
  val SEED: String = "seed"
  val CONFIGENTRYNAME: String = "Gemceas"
  val GENERATORCONFIGENTRYNAME: String = "Generator"
  val CODECONSTRAINTSCONFIGENTRYNAME: String = "CodeConstraints"

  val DEBUGPROGRAMGENERATION: String = "debugProgramGeneration"
  val DEBUGPROGRAMGENERATIONDEFAULT: Boolean = true
  val DEBUGPRODUCTIONRULES: String = "debugProductionRules"
  val DEBUGPRODUCTIONRULESDEFAULT: Boolean = true
  val DEBUGLEXERTOKENS: String = "debugLexerTokens"
  val DEBUGLEXERTOKENSDEFAULT: Boolean = true
  val GRAMMARMAXDEPTHREWRITING = "grammarMaxDepthRewriting"
  val GRAMMARMAXDEPTHREWRITINGDEFAULT: Int = 10
  val MAXDEPTHMULTIPLIER = "maxDepthMultiplier"
  val MAXDEPTHMULTIPLIERDEFAULT: Int = 10
  val MAXREPEATCONSTRUCT = "maxRepeatConstruct"
  val MAXREPEATCONSTRUCTDEFAULT: Int = 10
  val PROLOGVERIFICATION: String = "prologVerification"
  val PROLOGVERIFICATIONDEFAULT: Boolean = true

  val STRINGLENGTHMIN: String = "stringLengthMin"
  val STRINGLENGTHMINDEFAULT: Int = 3
  val STRINGLENGTHMAX: String = "stringLengthMax"
  val STRINGLENGTHMAXDEFAULT: Int = 10
  val INTLENGTHMIN: String = "intLengthMin"
  val INTLENGTHMINDEFAULT: Int = 1
  val INTLENGTHMAX: String = "intLengthMax"
  val INTLENGTHMAXDEFAULT: Int = 3
  val FLOATLENGTHMIN: String = "floatLengthMin"
  val FLOATLENGTHMINDEFAULT: Int = 1
  val FLOATLENGTHMAX: String = "floatLengthMax"
  val FLOATLENGTHMAXDEFAULT: Int = 3

  val OUTPUTDIRECTORY = "outputDirectory"
  def OUTPUTFILENAME: String =
    val currentDate = new Date(System.currentTimeMillis())
    val df = new SimpleDateFormat("dd-MM-yy-HH-mm-ss")
    "Gemceas_" + df.format(currentDate) + ".txt"

  val globalConfig: Config = obtainConfigModule(config, CONFIGENTRYNAME)

  val configGenerator: Config = obtainConfigModule(globalConfig, GENERATORCONFIGENTRYNAME)

  val configCodeConstraints: Config = obtainConfigModule(globalConfig, CODECONSTRAINTSCONFIGENTRYNAME)

  def obtainConfigModule(cf: Config, moduleName: String): Config = scala.util.Try(cf.getConfig(moduleName)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $moduleName: ${exception.getMessage}")
  }