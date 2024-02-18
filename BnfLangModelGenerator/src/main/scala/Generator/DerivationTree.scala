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

    /*
    * This function is used to resolve meta variables in the grammar.
      Consider the following grammar:
      nt0 ::= nt1
      nt1 ::= "t1" ntx "==>> nt1_f(T1, Ntx)" |
              nt2 "==>> nt1_f(Nt2)"
      nt2 ::= "t2" nty "mv =:= nt1.ntx" "==>> f(_, p2, mv)"
      ntx ::= "tx" nt1 "==>> ntx_f(Tx, Nt1)" |
              "" "==>> ntx_f(Terminal)"
      nty :: "ty" "==>> nty_f(Ty)" | nt1
      One of the possible rewriting paths is the following:
      nt0 =>
         nt1 =>
            "t1" ntx =>
                    "t1" "tx" nt1 =>
                              "t1" "tx" "t1" ntx =>
                                               "t1" "tx" "t1" "tx" nt1 =>
                                                                     "t1" "tx" "t1" "tx" nt2 =>
                                                                                 "t1" "tx" "t1" "tx" "t2" nty => // this is where mv is defined
                                                                                                                "t1" "tx" "t1" "tx" "t2" "ty"
      where the derivation tree contains the meta variable mv that references nt1 => ntx where nt1 is the parent of ntx and it should be the closest
      relatives in the obtained derivation tree. The parent of the gel mv is nt2, hence resolving this meta variable starts at this level
      with the search for ntx and since it is not present on the same level with mv then the search continues with the parent of nt2, i.e., ntx.
      The name, ntx matches the end gel of the definition, so the next step is to determine if the parent of ntx is nt1. If it is, then the
      search is over and the meta variable is resolved. If it is not, then the search continues with the parent of nt1.

      In case there are multiple gels ntx under nt1 the path expression can contain ._N where N is the index of the gel ntx. If N is less or equal to
      the number of gels ntx under nt1 then mv is resolved to the specific ntx. If N is greater than the number of gels ntx under nt1 then an error
      message is logged and the first gel ntx is chosen for resolution.
    * */
