package Utilz

import Utilz.Constants.{Dot, DotSeparator, MetaVariable_Assignment_Designator}
object MetaVariableManager:
  private val logger = CreateLogger(classOf[MetaVariableManager.type])

  def isMetaVariable(mVarDef: String): Option[(String, String)] =
    if mVarDef.contains(MetaVariable_Assignment_Designator) then
      val theSplit = mVarDef.trim.split(MetaVariable_Assignment_Designator)
      if theSplit.length == 3 then Option(theSplit(0).trim, theSplit(2).trim) else None
    else None

  def apply(mVarDef: String): Option[(String,List[String])] =
    if mVarDef.contains(MetaVariable_Assignment_Designator) then
      val theSplit = mVarDef.trim.split(MetaVariable_Assignment_Designator)
      if theSplit.length == 2 then
        val ntPath = theSplit(1).trim
        Option(theSplit(0).trim, if ntPath.contains(Dot) then ntPath.split(DotSeparator).toList.map(_.trim) else List(ntPath))
      else None
    else None

  @main def runMain_MetaVarExtractor(): Unit =
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= ").toString)
    logger.info(MetaVariableManager("=:= ").toString)
