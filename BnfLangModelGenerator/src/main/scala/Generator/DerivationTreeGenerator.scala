package Generator

import Compiler.BnFGrammarIR

import java.util.UUID

case class DTNode(
  id: String,
  children: Option[List[DTNode]]
)
trait DerivationTreeGenerator:
  val (theRoot, parentChildMap, allGels) = DerivationTree.outputTreeStructures()
  def builtTheTree(gel: BnFGrammarIR = theRoot, marker: Int = 0): DTNode = {
    val children: List[BnFGrammarIR] = if parentChildMap.contains(gel.uuid) then parentChildMap(gel.uuid).flatMap(e => if allGels.contains(e) then List(allGels(e)) else List[BnFGrammarIR]()) else List[BnFGrammarIR]()
    val nodeName = s"${gel.theName}_$marker"
    if children.isEmpty then DTNode(nodeName, None)
    else
      val cDT = children.map(builtTheTree(_, marker + 1))
      DTNode(nodeName, Some(cDT))
  }