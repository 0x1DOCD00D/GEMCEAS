/*******************************************************************************
 *  Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 ******************************************************************************/

package Randomizer

import Randomizer.SupplierOfRandomness.logger
import Utilz.CreateLogger
import org.slf4j.Logger

import scala.math.pow
import scala.util.Random

trait Randomizer(seed: Option[Long]):
  val generator: Random = seed match
  case Some(s) => Random(s)
  case None => Random()


class UniformProbGenerator(val seed: Option[Long] = None, minv:Int = 0, maxv:Int = Int.MaxValue) extends Randomizer(seed):
  val logger: Logger = CreateLogger(this.getClass)
  type GeneratedValues = Double | Int
  private def generateUniformProbabilities: LazyList[Double] = generator.nextDouble() #:: generateUniformProbabilities
  private def generateInts: LazyList[Int] = generator.between(minv,maxv) #:: generateInts
  private def uniformOrInts(ints: Boolean): () => LazyList[GeneratedValues] =
    if ints then () => generateInts else () => generateUniformProbabilities

object UniformProbGenerator:
  def createGenerator(seed: Option[Long] = None, minv:Int = 0, maxv:Int = Int.MaxValue): UniformProbGenerator =
    new UniformProbGenerator(seed, minv, maxv)
  def apply(gen: UniformProbGenerator, offset:Int = 0, szOfValues: Int = 1, ints: Boolean = false): (UniformProbGenerator, Int, List[Double|Int]) =
    if offset > Int.MaxValue - szOfValues then
      val newgen = createGenerator(Option(gen.seed.getOrElse(0L)+1))
      (newgen, szOfValues, newgen.uniformOrInts(ints)().slice(0, 0 + szOfValues).toList)
    else
      (gen, offset + szOfValues, gen.uniformOrInts(ints)().slice(offset, offset + szOfValues).toList)

  def onDemand(gen: UniformProbGenerator, minv:Int = 0, maxv:Int = Int.MaxValue): Int = gen.generator.between(minv,maxv)


