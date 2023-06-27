/*******************************************************************************
 *  Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

package Randomizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniformProbGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "random number generator"

  it should "create a uniform distribution of double precision values, take ten random values check the first value" in {
    val (gen, offset, values) = UniformProbGenerator(UniformProbGenerator.createGenerator(Some(1)), 0, 10)
    values.length shouldBe 10
    values.head shouldBe 0.7308781907032909
  }

  it should "create a uniform integer distribution, take ten random values check the first value" in {
    val (gen, offset, values) = UniformProbGenerator(UniformProbGenerator.createGenerator(Some(1)), 0, 10, true)
    values.length shouldBe 10
    values.head shouldBe 1569548985
  }

  it should "create a uniform distribution, take three and then take one random values and check it" in {
    val (gen, offset, values) = UniformProbGenerator(UniformProbGenerator.createGenerator(Some(2)), 0, 3)
    values.length shouldBe 3
    values.head shouldBe 0.7311469360199058
    val (gen2, offset2, values2) = UniformProbGenerator(gen, offset)
    values2.length shouldBe 1
    values2.head shouldBe 0.2281579303734177
    val (gen3, offset3, values3) = UniformProbGenerator(UniformProbGenerator.createGenerator(Some(2)), 0, 10)
    values3.length shouldBe 10
    values.head shouldBe values3.head
  }

}
