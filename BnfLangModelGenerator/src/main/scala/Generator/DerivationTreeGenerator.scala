package Generator

import Compiler.BnFGrammarIR

import java.util.UUID

trait DTAbstractNode:
  val id: String
  val children: Option[List[DTAbstractNode]]
  def asciiTreeRepresentation(indent: Int = 0): String
case object DTEmptyNode extends DTAbstractNode:
  override val id: String = ""
  override val children: Option[List[DTAbstractNode]] = None

  def asciiTreeRepresentation(indent: Int = 0): String = ""
case class DTNode(
  override val id: String,
  override val children: Option[List[DTAbstractNode]]
) extends DTAbstractNode:
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

  def builtTheTree(gel: BnFGrammarIR = theRoot, full: Boolean = true, nodeDiscriminator: BnFGrammarIR => Boolean = n => true): DTAbstractNode = {
    marker += 1
    val children: List[BnFGrammarIR] = if parentChildMap.contains(gel.uuid) then parentChildMap(gel.uuid).flatMap(e => if allGels.contains(e) then List(allGels(e)) else List[BnFGrammarIR]()) else List[BnFGrammarIR]()
    val nodeName = if full then gel.toString else s"${gel.theName}_$marker"
    if children.isEmpty then
      if nodeDiscriminator(gel) then DTNode(nodeName, None) else DTEmptyNode
    else
      val cDT = children.map(builtTheTree(_))
        .flatMap(e => if e.isInstanceOf[DTNode] then Some(e.asInstanceOf[DTNode]) else None)
      DTNode(nodeName, Some(cDT))
  }