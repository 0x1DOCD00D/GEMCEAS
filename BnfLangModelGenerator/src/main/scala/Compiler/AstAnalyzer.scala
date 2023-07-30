/*
 Copyright (c) 7/22/23, 7:01 PM by Mark Grechanik (drmark) and Lone Star Consulting, Inc. All rights reserved.

 Licensed under the Apache License, Version 2.0 (the “License”); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package Compiler

import Utilz.CreateLogger

import scala.collection.mutable.ListBuffer

class AstAnalyzer private (ast: List[ProductionRule]):
  private val logger = CreateLogger(classOf[AstAnalyzer])

  private def prRhsProcessor(rhs: BnFGrammarIR): List[BnfLiteral] =
    rhs match
      case container: BnFGrammarIRContainer =>
        val (bnfLit, bnfElse) = container.bnfObjects.partition(_.isInstanceOf[BnfLiteral])
        bnfLit.asInstanceOf[List[BnfLiteral]] ::: (bnfElse match
          case ::(head, next) => (head match
            case container: BnFGrammarIRContainer => prRhsProcessor(container)
            case literal: BnfLiteral => List(literal)
            ) ::: next.foldLeft(List[BnfLiteral]())((acc,e)=> prRhsProcessor(e):::acc)
          case Nil => List()
          )
      case ltrl: BnfLiteral => List(ltrl)
      case err =>
        logger.error(s"The rhs of the prod rule cannot be processed: ${err.toString}")
        List()
  end prRhsProcessor

  private def prodRuleProcessor(pr: ProductionRule): Option[(BnfLiteral, List[BnfLiteral])] =
    pr.lhs match
      case literal: BnfLiteral => Option((literal, prRhsProcessor(pr.rhs)))
      case err =>
        logger.error(s"Lhs of the prod rule ${pr.toString}.toString}")
        None

  end prodRuleProcessor

  private def extractLhs2RhsMappings(): List[Option[(BnfLiteral, List[BnfLiteral])]] = ast match
    case ::(head, next) => prodRuleProcessor(head) :: next.foldLeft(List[Option[(BnfLiteral, List[BnfLiteral])]]()) {
      (acc, rl) => prodRuleProcessor(rl) :: acc
    }
    case Nil => List()
  end extractLhs2RhsMappings

  private def obtainDims(mappings: List[Option[(BnfLiteral, List[BnfLiteral])]]): (List[BnfLiteral], List[BnfLiteral]) =
    def constructSetOfLiterals(l: List[BnfLiteral], resSet: Set[BnfLiteral]): Set[BnfLiteral] =
      l match
        case ::(head, next) => constructSetOfLiterals(next, resSet + head)
        case Nil => resSet
    end constructSetOfLiterals

    val rhs: Set[BnfLiteral] = Set.empty
    (mappings.flatten.map(_._1),
      mappings.flatten.map(e=>constructSetOfLiterals(e._2, Set.empty)).foldLeft(Set[BnfLiteral]())((acc,v)=> acc ++ v).toList
    )
  end obtainDims

  private def usageMatrix(mappings: List[(BnfLiteral, List[BnfLiteral])], dims: (List[BnfLiteral], List[BnfLiteral])): Array[Array[Int]] =
    val matrx: Array[Array[Int]] = Array.fill(dims._1.length)(Array.fill(dims._2.length)(0))
    mappings.foreach {
      p =>
        val row = dims._1.indexOf(p._1)
        p._2.foreach {
          e =>
            val col = dims._2.indexOf(e)
            matrx(row)(col) = matrx(row)(col) + 1
        }
    }
    matrx
  end usageMatrix

  private def toCsv(dims: (List[BnfLiteral], List[BnfLiteral]), m: Array[Array[Int]]): List[String] =
    val header = dims._2.mkString(",")
    val rowLen = dims._2.length
    val pm = m.zipWithIndex.map((row, ir) => dims._1(ir).token + ", " + row.zipWithIndex.map((col, ic) => s"${m(ir)(ic)}").mkString(", ")).toList
    pm
  end toCsv


end AstAnalyzer


object AstAnalyzer:
  def apply(ast: List[ProductionRule]) =
    val aStAn = new AstAnalyzer(ast)
    val rM = aStAn.extractLhs2RhsMappings()
    val dims = aStAn.obtainDims(rM)
    val matrix = aStAn.usageMatrix(rM.flatten, dims)
    aStAn.logger.info(dims._2.map(_.token).mkString(","))
    aStAn.toCsv(dims, matrix).foreach{
      l =>
        aStAn.logger.info(l)
    }
/*    aStAn.logger.info(dims._1.mkString(", "))
    aStAn.logger.info(dims._2.mkString(", "))
*/
  end apply


  @main def runMain_AstAnalyzer(): Unit =
    import LiteralType.*
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */

    val ast = List(

      ProductionRule(BnfLiteral("expression", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("sum_sub", NONTERM))
          ))
        )
      ),
      ProductionRule(BnfLiteral("sum_sub", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            BnfLiteral("product_div", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("+", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("-", TERM))
                    ))
                  ))
                ),
                BnfLiteral("product_div", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      product_div ::= ["+"|"-"] term {("*"|"/") term};
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("product_div", NONTERM),
        SeqConstruct(List(
          GroupConstruct(List(
            OptionalConstruct(List(
              UnionConstruct(List(
                GroupConstruct(List(
                  BnfLiteral("+", TERM))
                ),
                GroupConstruct(List(
                  BnfLiteral("-", TERM))
                ))
              ))
            ),
            BnfLiteral("term", NONTERM),
            RepeatConstruct(List(
              GroupConstruct(List(
                GroupConstruct(List(
                  UnionConstruct(List(
                    GroupConstruct(List(
                      BnfLiteral("*", TERM))
                    ),
                    GroupConstruct(List(
                      BnfLiteral("/", TERM))
                    ))
                  ))
                ),
                BnfLiteral("term", NONTERM))
              ))
            ))
          ))
        )
      ),
      /*
      term ::= number | "(" expression ")";
      <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
      */
      ProductionRule(
        BnfLiteral("term", NONTERM),
        SeqConstruct(List(
          UnionConstruct(List(
            GroupConstruct(List(
              BnfLiteral("number", NONTERM))
            ),
            GroupConstruct(List(
              BnfLiteral("(", TERM),
              BnfLiteral("expression", NONTERM),
              BnfLiteral(")", TERM))
            ))
          ))
        )
      ),
      ProductionRule(
        BnfLiteral("<number>", NTREGEX),
        BnfLiteral("""(\+|\-)?[0-9]+(\.[0-9]+)?""", REGEXTERM)
      )
    )
    AstAnalyzer(ast)
    /*
    expression ::= sum_sub;
    sum_sub ::= product_div {("+"|"-") product_div};
    product_div ::= ["+"|"-"] term {("*"|"/") term};
    term ::= number | "(" expression ")";
    <number> ::= "(\+|\-)?[0-9]+(\.[0-9]+)?";
    */

