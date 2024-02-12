package Generator

import Compiler.BnFGrammarIR
import Utilz.CreateLogger

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object RewritingTree:
  private val logger = CreateLogger(classOf[RewritingTree])
  private var theRoot: Tree = NilTree
  private val parentChildMap: Array[mutable.Map[UUID, List[UUID]]] = Array(mutable.Map(), mutable.Map())
  private val theTree: Array[ListBuffer[RewritingTree]] = Array(ListBuffer(), ListBuffer())

  def resetPrologFact(): Unit =
    parentChildMap(1) = mutable.Map()
    theTree(1).clear()

  def resetAll(): Unit =
    theRoot = NilTree
    parentChildMap(0) = mutable.Map()
    parentChildMap(1) = mutable.Map()
    theTree(0).clear()
    theTree(1).clear()
  def setTheRoot(gel: BnFGrammarIR): Either[String, RewritingTree] =
    if theRoot != NilTree then
      Left(s"Root already exists: $theRoot")
    else
      theRoot = RewritingTree(gel)
      logger.info(s"Root node set: $theRoot")
      parentChildMap(0) += (theRoot.asInstanceOf[RewritingTree].uuid -> List())
      Right(theRoot.asInstanceOf[RewritingTree])

  def addGrammarElements(gels: List[BnFGrammarIR], parent: RewritingTree, tempPrologFactTree: 0|1): Either[String, List[BnFGrammarIR]] =
    if theRoot == NilTree then
      Left("Root node does not exist")
    else if parentChildMap(tempPrologFactTree).contains(parent.uuid) then
      val children = parentChildMap(tempPrologFactTree)(parent.uuid)
      val newChildren = gels.map(RewritingTree(_))
      theTree(tempPrologFactTree) ++= newChildren
      newChildren.foreach { potentialParent =>
        parentChildMap(tempPrologFactTree) += (potentialParent.uuid -> List())
      }
      parentChildMap(tempPrologFactTree) += (parent.uuid -> (newChildren.map(_.uuid) ::: children))
      Right(gels)
    else
      Left(s"Parent not found: $parent")

sealed trait Tree
case class RewritingTree(gel: BnFGrammarIR, uuid: UUID = UUID.randomUUID()) extends Tree
case object NilTree extends Tree