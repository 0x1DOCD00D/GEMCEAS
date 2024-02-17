package Generator

import Compiler.{BnFGrammarIR, NonExistentElement}
import Utilz.CreateLogger

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object DerivationTree:
  type MainRewritingTree = 0
  type TempPrologFactRewritingTree = 1

  private val logger = CreateLogger(classOf[DerivationTree.type])
  private [this] var theRoot: BnFGrammarIR = NonExistentElement
  private [this] val parentChildMap: Array[mutable.Map[UUID, List[UUID]]] = Array(mutable.Map(), mutable.Map())
  private [this] val theTree: Array[ListBuffer[BnFGrammarIR]] = Array(ListBuffer(), ListBuffer())

  def resetPrologFact(): Unit =
    parentChildMap(1) = mutable.Map()
    theTree(1).clear()

  def resetAll(): Unit =
    theRoot = NonExistentElement
    parentChildMap(0) = mutable.Map()
    parentChildMap(1) = mutable.Map()
    theTree(0).clear()
    theTree(1).clear()
  def setTheRoot(gel: BnFGrammarIR): Either[String, BnFGrammarIR] =
    if theRoot != NonExistentElement then
      Left(s"Root already exists: $theRoot")
    else
      theRoot = gel
      logger.info(s"Root node set: $theRoot")
      parentChildMap(0) += (theRoot.uuid -> List())
      Right(theRoot)

  def addGrammarElements(gels: List[BnFGrammarIR], parent: BnFGrammarIR, tempPrologFactTree: MainRewritingTree | TempPrologFactRewritingTree): Either[String, List[BnFGrammarIR]] =
    if theRoot == NonExistentElement then
      Left("Root node does not exist")
    else if parentChildMap(tempPrologFactTree).contains(parent.uuid) then
      val children = parentChildMap(tempPrologFactTree)(parent.uuid)
      parentChildMap(tempPrologFactTree) += (parent.uuid -> (children ::: gels.map(_.uuid)))
      theTree(tempPrologFactTree) ++= gels
      gels.foreach { potentialParent =>
        parentChildMap(tempPrologFactTree) += (potentialParent.uuid -> List())
      }
      Right(gels)
    else
      Left(s"Parent not found: $parent")
