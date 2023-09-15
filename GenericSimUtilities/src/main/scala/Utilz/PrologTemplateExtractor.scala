/*
 Copyright (c) 8/25/23, 1:06 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

import Utilz.Constants.{CloseParen, CommaSeparator, OpenParen, Prolog_Template_Assignment, Prolog_Template_Designator}

/*
"==>> product_div(_, _, second_term(SecondTermSign, SecondTerm))"
*/

case class PrologTemplate(functorName: String, params: List[PrologTemplate])
class PrologTemplateExtractor(functor: String):
  import PrologTemplateExtractor.logger
  def parseFunctor(): Option[PrologTemplate] =
    val ind = functor.indexOf(OpenParen)
    if ind > 0 then
      val indCloseParen = functor.lastIndexOf(CloseParen)
      if indCloseParen > 0 then
        logger.info(s"Parsing functor $functor with the index of ( at $ind and the index of ) at $indCloseParen")
        val (fname, fparm) = (functor.substring(0, ind), functor.substring(ind+1, indCloseParen))
        logger.info(s"The param list for the functor $fname is $fparm")
        Some(PrologTemplate(fname, parseArguments(fparm).reverse))
      else None
    else
      Some(PrologTemplate(functor, List()))

  private def parseArguments(args: String): List[PrologTemplate] =
    (args.length :: args.zipWithIndex.toList.foldLeft((List[Int](), 0)) {
      (acc, e) =>
        if e._1 == CommaSeparator && acc._2 == 0 then (e._2 :: acc._1, acc._2)
        else if e._1 == OpenParen then (acc._1, acc._2 + 1)
        else if e._1 == CloseParen then (acc._1, acc._2 - 1)
        else acc
    }._1).reverse.foldLeft((0, List[String]())) {
      (acc, sepInd) =>
        val arg = args.substring(acc._1, sepInd).trim
        logger.info(s"extracted substring $arg from ${acc._1} to $sepInd")
        (sepInd+1, arg :: acc._2)
    }._2.flatMap(v => new PrologTemplateExtractor(v).parseFunctor())

object PrologTemplateExtractor:
  private val logger = CreateLogger(classOf[PrologTemplateExtractor])

  def isPrologTemplate(pTemplate: String): Option[String] =
    if pTemplate.contains(Prolog_Template_Designator) then
      val theSplit = pTemplate.trim.split(Prolog_Template_Designator)
      //    only the functor name and the param string are expected after the split using the designator
      if theSplit.length == 2 then Option(theSplit(1).trim) else None
    else if pTemplate.contains(Prolog_Template_Assignment) then
      val theSplit = pTemplate.trim.split(Prolog_Template_Assignment)
      if theSplit.length == 2 then Option(theSplit(1).trim) else None
    else None
  
  def apply(pTemplate: String): Option[PrologTemplate] =
    val template: Option[String] = isPrologTemplate(pTemplate)
    if template.isDefined then
      //      and we're interested in the functor itself
      new PrologTemplateExtractor(template.get).parseFunctor()
    else
      logger.error(s"Incorrect specification of the prolog template is provided $pTemplate")
      None

  @main def runMain_PrologTemplateExtractor(): Unit =
    logger.info(PrologTemplateExtractor("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier").toString)
    logger.info(PrologTemplateExtractor("==>> product_div(_, _, second_term(SecondTermSign, otherTerm(x)), final_term(n(m), y))").toString)

