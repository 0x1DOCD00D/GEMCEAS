package Utilz

import Utilz.Constants.{Dot, DotSeparator, MetaVariable_Assignment_Designator}

object MetaVariableManager:
  private val logger = CreateLogger(classOf[MetaVariableManager.type])

  def isMetaVariable(mVarDef: String): Boolean = if mVarDef.trim.contains(MetaVariable_Assignment_Designator) then true else false

  def apply(mVarDef: String): Option[(String, List[String], Option[Int])] =
    if isMetaVariable(mVarDef) then
      val theSplit = mVarDef.trim.split(MetaVariable_Assignment_Designator)
      if theSplit.length == 2 then
        val ntPath = theSplit(1).trim
        val mvName = theSplit(0).trim
        val mvPath = if ntPath.contains(Dot) then ntPath.split(DotSeparator).toList.map(_.trim) else List(ntPath)
        if mvName.isEmpty || mvPath.isEmpty then
          logger.error(s"MetaVariableManager:apply: Invalid MetaVariable Definition: $mVarDef")
          None
        else
          val indexPattern = """(_\d+)""".r
          mvPath.last match
            case indexPattern(ind) => Option(mvName, mvPath.init, Some(ind.substring(1).toInt))
            case _ => Option(mvName, mvPath, None)

      else
        logger.error(s"MetaVariableManager:apply: Invalid MetaVariable Definition: $mVarDef")
        None
    else None
  
  @main def runMain_MetaVarExtractor(): Unit =
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier._3").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier.__3").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= NormalClassDeclaration.TypeIdentifier._").toString)
    logger.info(MetaVariableManager("ClassIdentifier=:= ").toString)
    logger.info(MetaVariableManager("=:= ").toString)
