package Utilz

import Utilz.Constants.{Dot, DotSeparator, MetaVariable_Assignment_Designator}
object MetaVariableManager:
  private val logger = CreateLogger(classOf[MetaVariableManager.type])

  def isMetaVariable(mVarDef: String): Boolean = if mVarDef.trim.contains(MetaVariable_Assignment_Designator) then true else false

  def apply(mVarDef: String): Option[(String,List[String])] =
    if isMetaVariable(mVarDef) then
      val theSplit = mVarDef.trim.split(MetaVariable_Assignment_Designator)
      if theSplit.length == 2 then
        val ntPath = theSplit(1).trim
        Option(theSplit(0).trim, if ntPath.contains(Dot) then ntPath.split(DotSeparator).toList.map(_.trim) else List(ntPath))
      else
        logger.error(s"MetaVariableManager:apply: Invalid MetaVariable Definition: $mVarDef")
        None
    else None

  @main def runMain_MetaVarExtractor(): Unit =
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= ").toString)
    logger.info(MetaVariableManager("=:= ").toString)
