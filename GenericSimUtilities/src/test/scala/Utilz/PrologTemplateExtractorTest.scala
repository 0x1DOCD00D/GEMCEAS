/*
 Copyright (c) 8/26/23, 7:45 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Utilz

import Utilz.PrologTemplateExtractor.logger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PrologTemplateExtractorTest extends AnyFlatSpec with Matchers {
  behavior of "the extractor for prolog templates in grammars"

  it should "obtain all functors and arguments" in {
    PrologTemplateExtractor("==>> product_div(_, _, second_term(SecondTermSign, otherTerm(x)), final_term(n(m), y))") shouldBe Some(
      PrologTemplate("product_div",
        List(PrologTemplate("_", List()), PrologTemplate("_", List()),
          PrologTemplate("second_term", List(PrologTemplate("SecondTermSign", List()), PrologTemplate("otherTerm", List(PrologTemplate("x", List()))))),
          PrologTemplate("final_term", List(PrologTemplate("n", List(PrologTemplate("m", List()))), PrologTemplate("y", List()))))
      )
    )
  }

  it should "issue errors for incorrect template syntax" in {
    PrologTemplateExtractor("==>> product_div(_, _,") shouldBe None
  }

}
