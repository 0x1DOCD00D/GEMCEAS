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

  val SEED: String = "seed"
  val CONFIGENTRYNAME: String = "Gemceas"
  val MODELCONFIGENTRYNAME: String = "LangModel"
  val COSTREWARDSCONFIGENTRYNAME: String = "CostRewards"

  val EPSILON: Double = 1E-3d
  val DEBUGPRODUCTIONRULES: String = "debugProductionRules"
  val DEBUGPRODUCTIONRULESDEFAULT: Boolean = true
  val DEBUGLEXERTOKENS: String = "debugLexerTokens"
  val DEBUGLEXERTOKENSDEFAULT: Boolean = true
  val EDGEPROBABILITY: String = "edgeProbability"
  val DEFAULTEDGEPROBABILITY: Double = 0.3d
  val NUMBEROFEXPERIMENTS: String = "numberOfExperiments"
  val NUMBEROFEXPERIMENTSDEFAULT: Int = 10
  val GRAPHWALKTERMINATIONPOLICYDEFAULT = "maxpathlength"
  val GRAPHWALKTERMINATIONPOLICY = "graphWalkTerminationPolicy"
  val GRAPHWALKNODETERMINATIONPROBABILITY = "graphWalkNodeTerminationProbability"
  val GRAPHWALKNODETERMINATIONPROBABILITYDEFAULT = 0.05d
  val OUTPUTDIRECTORY = "outputDirectory"
  def OUTPUTFILENAME: String =
    val currentDate = new Date(System.currentTimeMillis())
    val df = new SimpleDateFormat("dd-MM-yy-HH-mm-ss")
    "Gemceas_" + df.format(currentDate) + ".txt"

  val SERVICEREWARD = "serviceReward"
  val SERVICEREWARDDEFAULT = 1.3d
  val SERVICEPENALTY = "servicePenalty"
  val SERVICEPENALTYDEFAULT = 2.3d
  val SERVICEREWARDPROBABILITY = "serviceRewardProbability"
  val SERVICEREWARDPROBABILITYDEFAULT = 0.5d

  val globalConfig: Config = obtainConfigModule(config, CONFIGENTRYNAME)

  val configLangModel: Config = obtainConfigModule(globalConfig, MODELCONFIGENTRYNAME)

  val configCostRewards: Config = obtainConfigModule(globalConfig, COSTREWARDSCONFIGENTRYNAME)

  def obtainConfigModule(cf: Config, moduleName: String): Config = scala.util.Try(cf.getConfig(moduleName)) match {
    case scala.util.Success(cfg) => cfg
    case Failure(exception) => throw new Exception(s"No config entry found for $moduleName: ${exception.getMessage}")
  }