package Generator

import Compiler.{BnFGrammarIR, BnfLiteral, MetaVariable, MetaVariableXformed, NonExistentElement, PrologFact, RepeatPrologFact}
import Utilz.CreateLogger

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Map

object DerivationTree:
  import GelChecks.*
  private type MainRewritingTree = 0
  private type TempPrologFactRewritingTree = 1

  private val logger = CreateLogger(classOf[DerivationTree.type])
  private [this] var theRoot: BnFGrammarIR = NonExistentElement
//  from parent nodes to leaves
  private [this] val parentChildMap: Array[mutable.Map[UUID, List[UUID]]] = Array(mutable.Map(), mutable.Map())
//from leaves to parent nodes
  private [this] val child2ParentMap: Array[mutable.Map[UUID, UUID]] = Array(mutable.Map(), mutable.Map())
//  a quick lookup of gels by their UUIDs
  private [this] val theTree: Array[mutable.Map[UUID, BnFGrammarIR]] = Array(mutable.Map(), mutable.Map())

  def outputTreeStructures(): (BnFGrammarIR, scala.collection.immutable.Map[UUID, List[UUID]], scala.collection.immutable.Map[UUID, BnFGrammarIR]) = (theRoot, parentChildMap(0).toMap, theTree(0).toMap)
  def resetPrologFact(pfRoot: Option[PrologFact] = None): Either[String, BnFGrammarIR] =
    parentChildMap(1) = mutable.Map()
    child2ParentMap(1) = mutable.Map()
    theTree(1).clear()
    pfRoot match
      case Some(pf) =>
        if parentChildMap(0).contains(pf.uuid) then
          parentChildMap(0) -= pf.uuid
          parentChildMap(1) += (pf.uuid -> List())
          theTree(1) += (pf.uuid -> pf)
        else
          parentChildMap(1) += (pf.uuid -> List())
        pf.add2DerivationTree() match
          case Left(throwEx) => Left(throwEx.getMessage)
          case Right(_) => Right(pf)
      case None => Left("Prolog fact is not defined")

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

  //the prolog fact tree is spliced under its parent
  def mergePFactTreeWithMainTree(): Either[String, BnFGrammarIR] =
    if theRoot == NonExistentElement then
      Left("Root node does not exist")
    else
      parentChildMap(0) ++= parentChildMap(1)
      theTree(0) ++= theTree(1)
      child2ParentMap(0) ++= child2ParentMap(1)
      Right(theRoot)

  def convertMetaVariable(gelHead: BnFGrammarIR, metaVar: MetaVariable): Either[String, List[BnFGrammarIR]] = {
    findMvReference(gelHead, metaVar, 1) match
      case Left(errMsg) =>
        Left(errMsg)
      case Right(mvXformed) =>
        MetaVarDictionary(mvXformed) match
          case Right(childrenRef) =>
            logger.info(s"Meta variable ${mvXformed.name} resolved to ${childrenRef.map(_.theName).mkString(", ")}")
            Right(childrenRef)
          case Left(errMsg) => Left(errMsg)
  }

  def addGrammarElements(gels: List[BnFGrammarIR], parent: BnFGrammarIR, tempPrologFactTree: MainRewritingTree | TempPrologFactRewritingTree): Either[String, List[BnFGrammarIR]] =
    val isParentInTree: BnFGrammarIR => Boolean = gel => parentChildMap(0).contains(gel.uuid) || parentChildMap(1).contains(gel.uuid)
    if theRoot == NonExistentElement then
      Left("Root node does not exist")
    else if isParentInTree(parent) then
      val children = parentChildMap(tempPrologFactTree)(parent.uuid)
      parentChildMap(tempPrologFactTree) += (parent.uuid -> (children ::: gels.map(_.uuid)))
      theTree(tempPrologFactTree) ++= gels.map(gel => gel.uuid -> gel)
      gels.foreach { gel =>
        parentChildMap(tempPrologFactTree) += (gel.uuid -> List())
        child2ParentMap(tempPrologFactTree) += (gel.uuid -> parent.uuid)
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

  private val multiplexBetweenThePfAndTheMainTree: BnFGrammarIR => Either[String, Int] = gel => {
    if child2ParentMap(0).contains(gel.uuid) then Right(0)
    else if child2ParentMap(1).contains(gel.uuid) then Right(1)
    else Left(s"Parent not found for $gel with id ${gel.uuid}")
  }

  private val childrenOfThePath: (BnFGrammarIR, Boolean) => List[BnFGrammarIR] = (gel, onlyPrologFact) => {
    multiplexBetweenThePfAndTheMainTree(gel) match
      case Left(errMsg) =>
        logger.error(errMsg)
        Nil
      case Right(index) =>
        parentChildMap(index).get(gel.uuid) match
          case Some(children) => children.flatMap(child =>
            theTree(index).get(child) match
              case Some(gel) => gel match
                case RepeatPrologFact(bnfObj,_) if !onlyPrologFact => bnfObj
                case PrologFact(_, mapParams2GrammarElements, _) => mapParams2GrammarElements.flatMap(_._2)
                case _ => if !onlyPrologFact then List(gel) else None
              case None => None
          )
          case None => Nil
  }

  @annotation.tailrec
  private def findTheLeafGel(leaf: String, parent: String, index: Option[Int], path: List[BnFGrammarIR]): Option[(BnFGrammarIR, BnFGrammarIR, List[BnFGrammarIR])] = {
    path match
      case leafGelParent :: tail =>
      // the name of the parent node should match
      // find all children nodes of the current leaf gel that match the leaf name
        multiplexBetweenThePfAndTheMainTree(leafGelParent) match
          case Left(errMsg) =>
            logger.error(errMsg)
            None
          case Right(branch) =>
            parentChildMap(branch).get(leafGelParent.uuid) match
              case Some(children) =>
                val leafGels = childrenOfThePath(leafGelParent, false).filter(child => isGelNt(child, Some(leaf)))
                if leafGels.isEmpty then
                  findTheLeafGel(leaf, parent, index, tail)
                else
                  if index.isDefined then
                    if index.get < leafGels.length then
                      Some((leafGels(index.get), leafGelParent, tail))
                    else
                      logger.error(s"Index $index is greater than the number of gels ${children.length}. Choosing the first gel.")
                      Some(leafGels.head, leafGelParent, tail)
                  else
                    leafGels.headOption match
                      case Some(gel) => Some(gel, leafGelParent, tail)
                      case None => findTheLeafGel(leaf, parent, index, tail)
              case None => None
      case Nil => None
  }

  private def matchTheRestOfThePath(foundMatch: (BnFGrammarIR, BnFGrammarIR, List[BnFGrammarIR]), specPath: List[String]): Either[String, Int] =
    val (leafGel, parent, remainingPath) = foundMatch
    if remainingPath.isEmpty && specPath.length > 0 then Left(s"Remaining path is exhausted and the path spec $specPath")
    else if remainingPath.isEmpty && specPath.isEmpty then Right(0)
    else if remainingPath.length < specPath.length then
      Left(s"Path length mismatch between the remaining path $remainingPath and the path spec $specPath")
    else
      if remainingPath.zip(specPath).forall {
        case (gel, name) =>
          childrenOfThePath(gel, true).exists(obj =>
          isGelNt(obj, Some(name)))
      }
      then
        Right(remainingPath.length)
      else
        Left(s"Path mismatch between the remaining path $remainingPath and the path spec $specPath")

  private def findMvReference(gelRef: BnFGrammarIR, mv: MetaVariable, tempPrologFactTree: MainRewritingTree | TempPrologFactRewritingTree): Either[String, MetaVariableXformed] = {
    @tailrec
    def getTreePath(ref: BnFGrammarIR, path:List[BnFGrammarIR]): Either[String, List[BnFGrammarIR]] = {
      if ref == theRoot then Right((ref :: path).reverse)
      else
        multiplexBetweenThePfAndTheMainTree(ref) match
          case Left(errMsg) =>
            logger.error(errMsg)
            Left(errMsg)
          case Right(index) =>
            val parent = child2ParentMap(index)(ref.uuid)
            if parent == theRoot.uuid then Right((theRoot :: path).reverse)
            else if !theTree(index).contains(parent) then
              Left(s"BnFElement doesn't exist for $ref")
            else
              val gep = theTree(index)(parent)
              if gep.isInstanceOf[BnfLiteral] then getTreePath(gep, gep :: path)
              else if childrenOfThePath(gep, false).exists(_.isInstanceOf[BnfLiteral]) then getTreePath(gep, gep :: path)
              else getTreePath(gep, path)
    }

    @tailrec
    def slideUpTheTreePath(leaf: String, parent: String, path: List[BnFGrammarIR]): Option[Int] = {
      if path.isEmpty then None
      else
        findTheLeafGel(leaf, parent, mv.index, path) match
          case Some(foundMatch) =>
            matchTheRestOfThePath(foundMatch, mv.path.reverse.tail) match
              case Left(errMsg) =>
                logger.info(errMsg)
                slideUpTheTreePath(leaf, parent, path.tail)
              case Right(theMatch) => Option(theMatch)
          case None => slideUpTheTreePath(leaf, parent, path.tail)
    }

    val fullPath = getTreePath(gelRef, List()) match
      case Left(errMsg) => logger.error(errMsg); Nil
      case Right(value) => value

//    TODO: the logic should be improved since the slideup is not always correct when working with prolog facts
//    TODO: only two path elements are considered, should be generalized to more, not difficult to do
    if fullPath.isEmpty then
      Left(s"Full path not found for $gelRef")
    else if mv.path.isEmpty then Left(s"Path not defined for $mv")
    else if mv.path.length < 2 then Left(s"Path length is less than 2 for $mv")
    else
      val leaf = mv.path.reverse.head
      val parent = mv.path.reverse.tail.head
      slideUpTheTreePath(leaf, parent, fullPath) match
        case None => Left(s"Path not found for $mv")
        case Some(theMatch) =>
          val foundGel = fullPath.lift(theMatch)
          if foundGel.isDefined then Right(MetaVariableXformed(mv.name, childrenOfThePath(foundGel.get, false)))
          else Left(s"Path not found for $mv")
  }