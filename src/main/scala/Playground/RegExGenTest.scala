/*
 Copyright (c) 8/6/23, 12:22 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.
  
 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
  
 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Playground

import Compiler.LiteralType.{REGEXTERM, TERM}
import Compiler.{BnfLiteral, ProgramEntity}

object RegExGenTest:
  import org.scalacheck.*
  import wolfendale.scalacheck.regexp.RegexpGen

  @main def runMain_RegExGenTest(): Unit =
    val generator: Gen[String] = RegexpGen.from("[-+]?[0-9]+(\\.[0-9]+)?")
      println(generator.sample)