package Utilities

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, IrLiteral, ProductionRule}
import Utilz.CreateLogger

import java.util.UUID

/*
 product_div ::=
  ["+"|"-"] term {("*"|"/") term "==>> term_repetition(Sign, Term)"}
  "PrevProductDiv =:= sum_sub.product_div._1"
  "==>> product_div(PrevProductDiv, _, NumberOrExpression, TermRepetition)";

  term ::=
    number
    "PrevTerm =:= product_div.term._2"

  Processing these grammar rules we locate product_div rule first by looking for a rule whose
  lhs contains an element nonterminal named product_div. Then we determine if there is an element named
  term in its rhs. If there are more than one element named term in its rhs then the next reference in the
  metavariable tells us which of these we should use.
* */

/*
* During rewriting we need to keep track of the metavariables that are used in the grammar rules. We maintain two
* tables, one where mv -> element and the other where element -> List[BnFGrammarIR]. The first table is used to
* determine the element that a metavariable refers to. The second table is used to determine the current rewrite of this element.
* Hence we create Map[String, BnFGrammarIR] and Map[BnFGrammarIR, List[BnFGrammarIR]].
* */
case class MetaVariableTableMgr(ast: List[ProductionRule]):
  private val logger = CreateLogger(classOf[MetaVariableTableMgr])
  private val mv2Element = scala.collection.mutable.Map[String, BnFGrammarIR]()
/*  
  def constructMVDictionary(obj2Find: BnFGrammarIR): List[BnFGrammarIR] = {
    def findGrammarObject(go: BnFGrammarIR): List[BnFGrammarIR] = go match
      case container: BnFGrammarIRContainer => if container.uuid == id then List(container) else container.bnfObjects.flatMap(findGrammarObject)
      case literal: IrLiteral => if literal.uuid == id then List(literal) else List()
      case ProductionRule(lhs, rhs) => findGrammarObject(lhs) ::: findGrammarObject(rhs)
      case err =>
        logger.error(s"findGrammarObject run into a wrong object: $err")
        List()
    end findGrammarObject

    ast.flatMap(findGrammarObject(_))
  }

*/