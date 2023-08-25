/*
 Copyright (c) 8/25/23, 1:06 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

/*
* val mainString = "==>> product_div(_, _, second_term(SecondTermSign, SecondTerm))"
val theSplit = mainString.trim.split("==>>")
if theSplit.length == 2 then
  val prologterm = theSplit(1)
  println(prologterm)
  val ind = prologterm.indexOf("(")
  val (fname, fparm) = (prologterm.substring(0, ind), prologterm.substring(ind+1, prologterm.length-1))
  println(fname + " =======>> " + fparm)

* */

case class PrologTemplate(functorName: String, params: List[String | PrologTemplate])
object PrologTemplateExtractor:
  def apply(pTemplate: String): PrologTemplate = ???