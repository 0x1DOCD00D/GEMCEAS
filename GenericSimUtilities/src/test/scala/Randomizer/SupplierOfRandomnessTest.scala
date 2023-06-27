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

class SupplierOfRandomnessTest extends AnyFlatSpec with Matchers {
  behavior of "supplier of the random numbers"

  it should "obtain ten random probs" in {
    val lst = SupplierOfRandomness.randProbs(10)
    lst.length shouldBe 10
    lst.head shouldBe 0.7304302967434272
    lst.sum shouldBe 5.255743899997139
  }

  it should "obtain one random integer within a given range" in {
    val intval = SupplierOfRandomness.onDemand(10,20)
    intval should be <= 20
    intval should be >= 10
  }
}
