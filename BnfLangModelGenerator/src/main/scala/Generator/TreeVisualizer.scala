package Generator

import Utilz.CreateLogger
import Utilz.ConfigDb.`Gemceas.outputDirectory`
import guru.nidi.graphviz.attribute.LinkAttr.weight
import guru.nidi.graphviz.attribute.{Color, Label}
import guru.nidi.graphviz.engine.{EngineResult, Format, Graphviz, GraphvizCmdLineEngine, GraphvizJdkEngine, GraphvizServerEngine}
import guru.nidi.graphviz.model.Factory.{graph, linkAttrs, node, to}
import guru.nidi.graphviz.model.{Graph, Node}
import org.slf4j.LoggerFactory

import java.io.File
import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}
class TreeVisualizer extends DerivationTreeGenerator:
  //  Use the following graphviz command to render the graph to an image:
  //  sfdp -x -Goverlap=scale -Tpng tree.dot > tree.png
  //`Gemceas.outputDirectory`
  private val logger = CreateLogger(classOf[TreeVisualizer])
  private def computeTreeGraph(tnd: DTAbstractNode, root: Boolean): List[Node] =
    val fNode: DTNode => Node = dt => if root then node(dt.id).`with`(Color.RED).`with`(Label.markdown(s"**${dt.id}**"), Color.rgb("1020d0").font()) else node(dt.id)
    val parent: Node = tnd match
      case DTEmptyNode => node("Empty")
      case DTNode(id, children) => fNode(tnd.asInstanceOf[DTNode])
    if tnd.children.isEmpty then List[Node]() else
      tnd.children.get.map(n => node(n.id)).foldLeft(List[Node]()) { case (acc, child) => parent.link(to(child).`with`(weight(2))) :: acc }
      ::: tnd.children.get.flatMap(computeTreeGraph(_, root = false))
  end computeTreeGraph

  def toAsciiStringFormat(fileName: String): Either[String, Long] =
    require(fileName.nonEmpty, "The file name cannot be empty")
    val dt = builtTheTree()
    val out = dt.asciiTreeRepresentation()
    //output to file
    Try {
      val file = new File(s"${`Gemceas.outputDirectory`}$fileName.txt")
      val bw = new java.io.BufferedWriter(new java.io.FileWriter(file))
      bw.write(out)
      bw.close()
    } match
      case Failure(exception) => Left(s"Failed to write the ascii tree to ${`Gemceas.outputDirectory`}$fileName.txt for reason ${exception.getMessage}")
      case Success(_) =>
        logger.info(s"Successfully wrote the ascii tree to ${`Gemceas.outputDirectory`}$fileName.txt")
        Right(new File(s"${`Gemceas.outputDirectory`}$fileName.txt").length())

  def toDotVizFormat(name: String, fileName: String, outputImageFormat: Format = Format.DOT): Either[String, Boolean] =
    require(name.nonEmpty, "The graph name cannot be empty")
    require(fileName.nonEmpty, "The file name cannot be empty")
    val dt = builtTheTree()
    val tree = computeTreeGraph(dt, root = true)

    val g = graph(name).directed().`with`(tree: _*).
      linkAttr().`with` ("class", "link-class").`with`(tree: _*)
    Try(new GraphvizCmdLineEngine()).map(cmdlnEngine => cmdlnEngine.timeout(2, TimeUnit.MINUTES)).map { cmdlnEngine =>
      Graphviz.useEngine(cmdlnEngine)
      Graphviz.fromGraph(g).render(Format.DOT).toFile(new File(s"${`Gemceas.outputDirectory`}$fileName.${Format.DOT.fileExtension}"))
    } match
      case Failure(exception) => Left(s"Failed to render the graph to ${`Gemceas.outputDirectory`}$fileName.${outputImageFormat.fileExtension} for reason ${exception.getMessage}")
      case Success(f) =>
          logger.info(s"Successfully rendered the graph to ${`Gemceas.outputDirectory`}$fileName.${outputImageFormat.fileExtension}")
          Right(f.exists())
  end toDotVizFormat