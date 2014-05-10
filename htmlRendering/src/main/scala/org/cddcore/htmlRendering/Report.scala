package org.cddcore.htmlRendering

import java.util.Date
import org.cddcore.engine._
import org.cddcore.utilities._
import StartChildEndType._
import EngineTools._
import java.io.File

object Report {
  def apply(title: Option[String] = None, date: Date = new Date, description: Option[String] = None, nodes: List[Reportable] = List()) =
    new SimpleReport(title, date, description, nodes)
  def documentAndEngineReport(title: Option[String], date: Date, engines: Traversable[Engine], description: Option[String] = None) =
    new DocumentAndEngineReport(title, date, engines, description)
  def engineReport(title: Option[String], date: Date, engine: Engine, description: Option[String] = None) =
    new FocusedReport(title, date, List(engine.asRequirement), description)
  def focusedReport(title: Option[String], date: Date, pathWithoutReport: List[Reportable], description: Option[String] = None) =
    new FocusedReport(title, date, pathWithoutReport, description)
  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String]): String = {
    val renderContext = RenderContext(UrlMap() ++ report.urlMapPaths, new Date())
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => html + engine(renderContext, path, cse) }
  }

  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String], renderContext: RenderContext): String =
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => html + engine(renderContext, path, cse) }

}

trait Report extends Requirement {
  def title: Option[String]
  def date: Date
  def description: Option[String]
  def priority: Option[Int] = None
  def references: Set[Reference] = Set()
  def reportPaths: List[List[Reportable]]
  def urlMapPaths: List[List[Reportable]] = reportPaths
}

trait ReportWriter {
  def print(url: String, main: Option[Reportable], html: String)
}

class FileReportWriter extends ReportWriter {
  def print(url: String, main: Option[Reportable], html: String) {
    val prefix = "file:///"
    if (url.startsWith(prefix)) {
      val path = url.substring(prefix.length())
      Files.printToFile(new File(path))((pw) =>
        pw.print(html))
    } else throw new IllegalArgumentException("Url is " + url)
  }
}

class MemoryReportWriter extends ReportWriter {
  var map: Map[String, (Option[Reportable], String)] = Map()
  def print(url: String, main: Option[Reportable], html: String) {
    if (map.contains(url)) throw new IllegalArgumentException(url + " is already in map")
    map = map + (url -> (main, html))
  }

}
class ReportOrchestrator(rootUrl: String, title: String, engines: List[Engine], date: Date = new Date, reportWriter: ReportWriter = new FileReportWriter) {
  import EngineTools._
  import Strings._
  val rootReport = Report.documentAndEngineReport(Some(title), date, engines)
  val engineReports = engines.foldLeft(List[Report]())((list, e) => Report.engineReport(Some("title"), date, e) :: list).reverse
  val urlMap = UrlMap(rootUrl) ++ rootReport.urlMapPaths
  val renderContext = RenderContext(urlMap, date)
  def makeReports = {
    val t = rootReport.reportPaths
    reportWriter.print(Strings.url(rootUrl, title, "index.html"), None, Report.html(rootReport, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext))
    for (e <- engines; path <- e.asRequirement.pathsIncludingSelf.toList) {
      val r = path.head
      println(r)
      val url = urlMap(r)
      val report = Report.focusedReport(Some("title"), date, path)
      val renderer = HtmlRenderer.rendererFor(r)
      val html = Report.html(report,renderer, renderContext)
      reportWriter.print(url, Some(r), html)
    }
  }
}

case class SimpleReport(
  val title: Option[String],
  val date: Date,
  val description: Option[String],
  val nodes: List[Reportable],
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  val reportPaths = pathsIncludingSelf.toList

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new SimpleReport(title, date, description, nodes, textOrder)

  override def toString = s"Report(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case p: SimpleReport => Requirement.areRequirementFieldsEqual(this, p) && nodes == p.nodes
    case _ => false
  }
}

case class DocumentAndEngineReport(title: Option[String],
  val date: Date,
  val engines: Traversable[Engine],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  import EngineTools._
  import ReportableHelper._

  val documents = engines.flatMap(_.asRequirement.documents).toList.removeDuplicates
  val sortedEngines = engines.toList.sortBy(_.textOrder)
  val documentHolder = DocumentHolder(documents)
  val engineHolder = EngineHolder(sortedEngines)
  val nodes = List(documentHolder, engineHolder)
  private val thisAsPath: List[Reportable] = List(this)
  private val thisAndEngineHolderAsPath: List[Reportable] = List(engineHolder, this)
  private val shortenginePaths: List[List[Reportable]] = engines.flatMap(_.asRequirement match {
    case e: EngineDescription[_, _, _, _] => List(e :: thisAndEngineHolderAsPath)
    case f: FoldingEngineDescription[_, _, _, _, _] => {
      val head = (f :: thisAndEngineHolderAsPath)
      val tail = f.nodes.map(_ :: f :: thisAndEngineHolderAsPath);
      val result = head :: tail
      result
    }
  }).toList
  private val fullEnginePaths: List[List[Reportable]] = engines.flatMap(_.asRequirement.pathsIncludingTree(thisAsPath)).toList
  override val urlMapPaths = List(thisAsPath) ++ documentHolder.pathsFrom(thisAsPath) ++ fullEnginePaths
  val reportPaths = List(thisAsPath) ++ documentHolder.pathsIncludingSelf(thisAsPath) ++ List(thisAndEngineHolderAsPath) ++ shortenginePaths
  //    reportPaths ++ engines.map(_.asRequirement).flatMap((ed) => ed.pathsIncludingTree(engineHolder :: ed :: this :: Nil))

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new DocumentAndEngineReport(title, date, engines, description, textOrder)

  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case de: DocumentAndEngineReport => Requirement.areRequirementFieldsEqual(this, de) && engines == de.engines
    case _ => false
  }
}

//case class EngineReport(title: Option[String],
//  val date: Date,
//  val engine: Engine,
//  val description: Option[String] = None,
//  val textOrder: Int = Reportable.nextTextOrder) extends Report {
//  import EngineTools._
//  import ReportableHelper._
//
//  val ed = engine.asRequirement
//  val basePath = List(engine.asRequirement, this)
//  val baseReportPaths: List[List[Reportable]] = List(this) :: basePath :: Nil
//  val reportPaths: List[List[Reportable]] = engine match {
//    case e: EngineFromTests[_, _, _, _] => baseReportPaths ::: ed.pathsFrom(basePath).toList ::: e.tree.treePathsWithElseClause(basePath)
//    case f: FoldingEngine[_, _, _, _, _] => baseReportPaths ::: f.engines.flatMap((e) => {
//      val ed = e.asRequirement
//      ed.pathsIncludingSelf(basePath).toList ::: e.tree.treePathsWithElseClause(ed :: basePath)
//    })
//  }
//
//  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
//    new EngineReport(title, date, engine, description, textOrder)
//
//  override def hashCode = (title.hashCode() + description.hashCode()) / 2
//  override def equals(other: Any) = other match {
//    case er: EngineReport => Requirement.areRequirementFieldsEqual(this, er) && engine == er.engine
//    case _ => false
//  }
//}

case class FocusedReport(title: Option[String],
  val date: Date,
  val focusPath: List[Reportable],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report {
  import EngineTools._
  import ReportableHelper._

  private val pathToFocus = focusPath :+ this
  private val pathsToFocus = Lists.decreasingList(pathToFocus).reverse
  val addDecisionTree = (path: List[Reportable]) => path match {
    case path @ (ed: EngineDescription[_, _, _, _]) :: tail => ed.tree.get.treePathsWithElseClause(path)
    case _ => List()
  }
  def childrenPaths(path: List[Reportable]): List[List[Reportable]] = path match {
    case (f: FoldingEngineDescription[_, _, _, _, _]) :: tail => f.nodes.flatMap {
      case ed: EngineDescription[_, _, _, _] => ed.pathsIncludingTree(path)
    }
    case (h: NestedHolder[Reportable]) :: tail => h.pathsFrom(path).toList
    case _ => List()
  }

  val reportPaths = pathsToFocus ::: childrenPaths(pathToFocus) ::: pathsToFocus.flatMap(addDecisionTree)

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FocusedReport(title, date, focusPath, description, textOrder)
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case fr: FocusedReport => Requirement.areRequirementFieldsEqual(this, fr) && focusPath == fr.focusPath
    case _ => false
  }
}

/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}