/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */
package Utilz

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.Failure
import com.github.dwickern.macros.NameOf.*

object Constants:
  final val Prolog_Template_Designator = "==>>"
  final val MetaVariable_Assignment_Designator = "=:="
  final val OpenParen = '('
  final val CloseParen = ')'
  final val OpenBra = "["
  final val CloseKet = "]"
  final val CommaSeparator = ','
  final val DotSeparator = "\\."
  final val Dot = "."
  final val ArgumentUnderscore = "_"

  val OUTPUTDIRECTORY = "outputDirectory"
  def OUTPUTFILENAME: String =
    val currentDate = new Date(System.currentTimeMillis())
    val df = new SimpleDateFormat("dd-MM-yy-HH-mm-ss")
    "Gemceas_" + df.format(currentDate) + ".txt"
