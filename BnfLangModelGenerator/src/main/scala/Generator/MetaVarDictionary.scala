package Generator

import scala.collection.mutable
import Compiler.{BnFGrammarIR, MetaVariableXformed}
import Utilz.CreateLogger
object MetaVarDictionary:
  private val logger = CreateLogger(classOf[MetaVarDictionary.type])
  private[this] val metaVarDict: mutable.Map[String, List[BnFGrammarIR]] = mutable.Map()

  def apply(mv: MetaVariableXformed): Either[String, List[BnFGrammarIR]] =
    if metaVarDict.contains(mv.name) then
      Left(s"MetaVarDictionary: ${mv.name} already exists, duplicate names aren't allowed")
    else
      metaVarDict += (mv.name -> mv.path)
      Right(mv.path)

  def apply(mvName: String): Option[List[BnFGrammarIR]] =
    if !metaVarDict.contains(mvName) then
      None
    else
      Some(metaVarDict(mvName))

  def checkMvNames(mvNames: List[String]): List[String] =
    mvNames.flatMap(mvName =>
      if !metaVarDict.contains(mvName) then
        List()
      else
        metaVarDict(mvName).map(_.theName)
    )
