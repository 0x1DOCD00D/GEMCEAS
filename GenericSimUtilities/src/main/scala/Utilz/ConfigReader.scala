/*******************************************************************************
 *  Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

package Utilz

import Utilz.GemceasConstants.{EnumeratedLoopParameters, FromToWithStepParameters}
import com.typesafe.config.Config

import scala.util.Try

object ConfigReader:
  def getConfigEntry[T](config: Config, entry: String, defValue: T): T =
    val cv = defValue match
      case v: Int => Try(config.getInt(entry))
      case v: Long => Try(config.getLong(entry))
      case v: Double => Try(config.getDouble(entry))
      case EnumeratedLoopParameters(ps) => Try(config.getDoubleList(entry))
      case FromToWithStepParameters(from, to, step) => Try(config.getDoubleList(entry))
      case _ => Try(config.getString(entry))

    cv match {
      case scala.util.Success(value) =>
        Try(value) match {
          case scala.util.Success(value) =>
            defValue match
              case _: EnumeratedLoopParameters => EnumeratedLoopParameters(value.asInstanceOf[List[Double]]).asInstanceOf[T]
              case _: FromToWithStepParameters =>
                if value.asInstanceOf[List[Double]].length == 3
                then FromToWithStepParameters(value.asInstanceOf[List[Double]].head,
                  value.asInstanceOf[List[Double]](1),
                  value.asInstanceOf[List[Double]](2)).asInstanceOf[T]
                else
                  defValue
              case _ => value.asInstanceOf[T]
          case scala.util.Failure(_) => defValue
        }
      case scala.util.Failure(_) => defValue
    }
