package Utilities

import Compiler.{BnFGrammarIR, BnFGrammarIRContainer, IrLiteral, ProductionRule}
import Utilz.CreateLogger

import java.util.UUID

case class BnFIrSearch(ast: List[ProductionRule]):
  private val logger = CreateLogger(classOf[BnFIrSearch])
  def findBnFObject(obj2Find: BnFGrammarIR): List[BnFGrammarIR] = {
    val id: UUID = obj2Find.uuid
    def findGrammarObject(go: BnFGrammarIR): List[BnFGrammarIR] = go match
      case container: BnFGrammarIRContainer => if container.uuid == id then List(container) else container.bnfObjects.flatMap(findGrammarObject)
      case literal: IrLiteral => if literal.uuid == id then List(literal) else List()
      case ProductionRule(lhs, rhs, _) => findGrammarObject(lhs) ::: findGrammarObject(rhs)
      case err =>
        logger.error(s"findGrammarObject run into a wrong object: $err")
        List()
    end findGrammarObject

    ast.flatMap(findGrammarObject(_))
  }
