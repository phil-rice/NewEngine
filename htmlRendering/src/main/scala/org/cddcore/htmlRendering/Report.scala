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
    val urlMap = UrlMap() ++ report.urlMapPaths
    val iconUrl = Strings.url(urlMap.rootUrl, report.titleString, "index.html")
    val renderContext = RenderContext(urlMap, new Date(), iconUrl)
    html(report, engine, renderContext)
  }

  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String], renderContext: RenderContext): String =
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => html + engine(renderContext, path, cse) }

}

trait Report extends TitledReportable {
  def title: Option[String]
  def date: Date
  def description: Option[String]
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
  val iconUrl = Strings.url(rootUrl, title, "index.html")
  val renderContext = RenderContext(urlMap, date, iconUrl)
  def makeReports = {
    val t = rootReport.reportPaths
    reportWriter.print(iconUrl, None, Report.html(rootReport, HtmlRenderer.engineAndDocumentsSingleItemRenderer, renderContext))
    for (e <- engines; path <- e.asRequirement.pathsIncludingSelf.toList) {
      val r = path.head
      println(r)
      val url = urlMap(r)
      val report = Report.focusedReport(Some("title"), date, path)
      val renderer = HtmlRenderer.rendererFor(r)
      val actualPathToConclusion = pathToConclusion(path)
      val newRenderContext = renderContext.copy(pathToConclusion = actualPathToConclusion)
      val html = Report.html(report, renderer, newRenderContext)
      reportWriter.print(url, Some(r), html) 
    } 
  }

  def pathToConclusion[Params, BFn, R, RFn](path: List[Reportable]): List[Reportable] = {
    def engineFromTestsFor(ed: EngineDescription[Params, BFn, R, RFn]) =
      engines.flatMap {
        case e: EngineFromTests[Params, BFn, R, RFn] if (e.asRequirement.eq(ed)) => Some(e)
        case f: FoldingEngine[Params, BFn, R, RFn, _] => f.engines.collect { case e: EngineFromTests[Params, BFn, R, RFn] if (e.asRequirement.eq(ed)) => e }.headOption
        case _ => None
      }.head
    def pathFrom(e: EngineFromTests[Params, BFn, R, RFn], params: Params) = e.evaluator.findPathToConclusion(e.tree, params)

    path.head match {
      case s: Scenario[Params, BFn, R, RFn] => path.collect { case ed: EngineDescription[Params, BFn, R, RFn] => pathFrom(engineFromTestsFor(ed), s.params) }.head
      case _ => List()
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

  override def toString = s"Report(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
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

}

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

}

/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}