package Generator

import Compiler.BnFGrammarIR

import java.util.UUID

case class DTNode(
  id: String,
  children: Option[List[DTNode]]
):
  def asciiTreeRepresentation(indent: Int = 0): String = {
    val indentStr = "   " * indent
    val childrenStr = children match
      case Some(c) =>
        val cStr = c.map(_.asciiTreeRepresentation(indent + 1)).mkString("\n")
        s"$indentStr$id {\n$cStr\n$indentStr }"
      case None => s"$indentStr$id"
    childrenStr
  }

trait DerivationTreeGenerator:
  val (theRoot, parentChildMap, allGels) = DerivationTree.outputTreeStructures()
  private var marker: Int = 0
  def builtTheTree(gel: BnFGrammarIR = theRoot, full: Boolean = true): DTNode = {
    marker += 1
    val children: List[BnFGrammarIR] = if parentChildMap.contains(gel.uuid) then parentChildMap(gel.uuid).flatMap(e => if allGels.contains(e) then List(allGels(e)) else List[BnFGrammarIR]()) else List[BnFGrammarIR]()
    val nodeName = if full then gel.toString else s"${gel.theName}_$marker"
    if children.isEmpty then DTNode(nodeName, None)
    else
      val cDT = children.map(builtTheTree(_))
      DTNode(nodeName, Some(cDT))
  }