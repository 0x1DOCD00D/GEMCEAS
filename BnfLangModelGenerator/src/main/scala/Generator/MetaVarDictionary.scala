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

  def apply(mvName: String): Either[String, List[BnFGrammarIR]] =
    if metaVarDict.contains(mvName) == false then
      Left(s"MetaVarDictionary: $mvName is not defined.")
    else
      Right(metaVarDict(mvName))
