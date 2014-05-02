package org.cddcore.htmlRendering

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem
import org.cddcore.cddjunit.CddJunitRunner
import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.cddcore.utilities._
import org.cddcore.utilities.StartChildEndType._
import org.junit.runner.RunWith
import java.text.DateFormat

object HtmlStrings {
  def report = ???
}

object ReportDetails {
  def apply() = new SimpleReportDetails
}
trait ReportDetails {
  def css: String
  def reportStart(title: String, date: Date): String
  def reportEnd: String
  def reportDateFormatter: DateFormat
}

class SimpleReportDetails(
  val css: String = Files.getFromClassPath(classOf[ReportDetails], "cdd.css"),
  val reportStartTemplate: String = Files.getFromClassPath(classOf[ReportDetails], "reportStart"),
  val reportEnd: String = Files.getFromClassPath(classOf[ReportDetails], "reportEnd"),
  val reportDateFormatter: DateFormat = new SimpleDateFormat("EEE, d MMM yyyy HH:mm")) extends ReportDetails {
  def reportStart(title: String, date: Date) =
    reportStartTemplate.replace("$REPORT_TITLE$", title).replace("$REPORT_DATE$", reportDateFormatter.format(date)).replace("$CSS$", css)
}
case class RenderContext(urlMap: UrlMap, reportDate: Date, reportDetails: ReportDetails = ReportDetails())

case class DocumentHolder(val nodes: List[Document], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable
case class EngineHolder(val nodes: List[Engine], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable

object DocumentAndEngineHolder {
  import EngineTools._
  import ReportableHelper._
  def apply(es: Iterable[Engine]) = {
    val documents = es.flatMap(_.asRequirement.documents).toList.removeDuplicates.sortBy(_.textOrder)
    val engines = es.toList.sortBy(_.textOrder)
    new DocumentAndEngineHolder(documents, engines)
  }
  def apply(e: Engine) = new DocumentAndEngineHolder(e.asRequirement.documents, List(e))
}

case class DocumentAndEngineHolder(documents: List[Document], val engines: List[Engine], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable {
  val documentHolder = DocumentHolder(documents)
  val engineHolder = EngineHolder(engines)
  val nodes = List(documentHolder, engineHolder)
  override def toString() = s"DocumentAndEngineHolder(docs=${documents.map(_.titleString).mkString(",")},engines=${engines.map(_.titleString).mkString(",")}"
}
object SampleContexts {

  val testDate = new Date(2000, 1, 1)
  val rootUrl = "RootUrl"
  val emptyUrlMap = UrlMap(rootUrl)
  def documentAndEngineReport(e: Engine) = {
    import ReportableHelper._
    import EngineTools._
    val ed = e.asRequirement
    val dAndE = DocumentAndEngineHolder(ed.documents, ed.engines)
    (Report(Some("reportTitle"), nodes = List(dAndE)), dAndE)

  }
  def context(rs: Reportable*) = {
    val c = RenderContext(Lists.decreasingList(rs).foldLeft(emptyUrlMap) { _ + _ }, testDate)
    c
  }

  def documentContext(d: Document, ds: Document*): (RenderContext, List[Reportable]) = {
    val dAndE = DocumentAndEngineHolder(List(d), List())
    val report = Report(Some("reportTitle"), nodes = List(dAndE))
    (RenderContext(emptyUrlMap ++ report, testDate), List(d, dAndE, report))
  }

  val eBlank = Engine[Int, Int]().build
  val engineHolderWithBlank = EngineHolder(List(eBlank), textOrder = 111)

  val eBlankWithTitle = Engine[Int, Int]().title("EBlankTitle").build
  val engineHolderWithBlankAndTitle = EngineHolder(List(eBlankWithTitle), textOrder = 222)

  val reqNoTitle = RequirementForTest(textOrder = 666)
  val reqWithTitle = RequirementForTest(title = Some("ReqTitle"))

  val doc1 = Document(title = Some("doc1title"), url = Some("doc1Url"))
  val docWithoutTitle = Document(url = Some("doc1Url"))
  val docHolderWithDoc1 = DocumentHolder(List(doc1), textOrder = 333)

  val documentAndEngineHolder = new DocumentAndEngineHolder(List(doc1), List(eBlankWithTitle), textOrder = 444)
}

@RunWith(classOf[CddJunitRunner])
object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._

  private def formatter = new SimpleDateFormat("dd MMM yyyy")

  type PathAndTag = (List[Reportable], StartChildEndType)
  def documentsAndEngine(title: String, engines: List[Engine], date: Date) = {
    val dAndE = DocumentAndEngineHolder(engines)
    val report = Report(Some(title), nodes = List(dAndE))
    val urlMap = UrlMap() ++ report
    val renderContext = RenderContext(urlMap, date)
    val css = Files.getFromClassPath(classOf[ReportDetails], "cdd.css")
    val html = Lists.pathToStartChildEnd(report.pathsIncludingSelf).foldLeft("") { case (acc, (path, tag)) => acc + engineAndDocumentsSingleItemRenderer(renderContext, path, tag) }
    html
  }
  import TemplateLike._

  def builderWithReport(title: String) = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title(title).
    useCase("Reports have a huge template at the start, and end. The report title and date are substituted in").
    scenario(context(), List(Report(title = Some("report title"), date = testDate)), Start).
    expected(ReportDetails().reportStart("report title", testDate)).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, Start) => reportDetails.reportStart(r.titleString, date) }.

    scenario(context(), List(Report(title = Some("report title"), date = testDate)), End).
    expected(ReportDetails().reportEnd).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, End) => reportDetails.reportEnd }

  val titleAndIcon = Engine[RenderContext, Reportable, String]().title("titleAndIcon").description("Finds a suitable titleAndIcon for a reportable. Includes links to go to item, and the id from the urlmap").
    useCase("Items that are requirements with titles use their titles").
    scenario(context(reqWithTitle), reqWithTitle).
    expected(s"<a id='RequirementForTest_${reqWithTitle.textOrder}' href='RootUrl/ReqTitle.RequirementForTest.html'>ReqTitle</a>").
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${r.titleString}</a>" }.

    useCase("Items that are requirements without titles are given template name and text order").
    scenario(context(doc1), doc1).
    expected(s"<a id='Document_${doc1.textOrder}' href='RootUrl/doc1title.Document.html'>doc1title</a>").
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${r.titleString}</a>" }.

    useCase("Engines are displayed based on their requirements. Without a name uses template name and text order").
    scenario(context(eBlank), eBlank).
    expected(s"<a id='Engine1FromTests_${eBlank.textOrder}' href='RootUrl/Engine1FromTests${eBlank.textOrder}.Engine1FromTests.html'></a>").
    matchOn { case (RenderContext(urlMap, _, _), r: Engine) => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${r.titleString}</a>" }.

    build

  val engineAndDocumentsSingleItemRenderer = builderWithReport("Engine and Documents Single Item Renderer").
    useCase("A Document And Engine Holder has a div").
    scenario(context(documentAndEngineHolder), List(documentAndEngineHolder), Start).
    expected("\n<div class='documentAndEngineHolder'>\n").
    because { case (_, (r: DocumentAndEngineHolder) :: _, Start) => true; case _ => false }.

    scenario(context(documentAndEngineHolder), List(documentAndEngineHolder), End).
    expected("\n</div> <!-- documentAndEngineHolder -->\n").
    because { case (_, (r: DocumentAndEngineHolder) :: _, End) => true; case _ => false }.

    useCase("Engine Holders have a div, and hold the items as an unorder list").
    scenario(context(engineHolderWithBlank), List(engineHolderWithBlank), Start).
    expected("\n<div class='engineHolder'><h3>Engines</h3><ul>\n").
    because { case (_, (holder: EngineHolder) :: _, Start) => true; case _ => false }.

    scenario(context(engineHolderWithBlank), List(engineHolderWithBlank), End).
    expected("\n</ul></div> <!-- engineHolder -->\n").
    because { case (_, (holder: EngineHolder) :: _, End) => true; case _ => false }.

    useCase("Document Holders have a div, and hold the items as an unorder list").
    scenario(context(docHolderWithDoc1), List(docHolderWithDoc1), Start).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul>\n").
    because { case (_, (holder: DocumentHolder) :: _, Start) => true; case _ => false }.

    scenario(context(docHolderWithDoc1), List(docHolderWithDoc1), End).
    expected("\n</ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, End) => true; case _ => false }.

    scenario(context(docHolderWithDoc1), List(docHolderWithDoc1), Child).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul><li>No documents</li></ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, Child) => true; case _ => false }.

    useCase("Documents are in an anchor, and use the TitleAndIcon engine").
    scenario(context(doc1, docHolderWithDoc1), List(doc1), Child).
    expected(s"\n<li>${titleAndIcon(context(doc1, docHolderWithDoc1), doc1)}</li>\n").
    matchOn { case (rc @ RenderContext(urlMap, _, _), ((doc: Document) :: _), Child) => s"\n<li>${titleAndIcon(rc, doc)}</li>\n" }.

    useCase("Engines are in an anchor").
    scenario(
      context(eBlankWithTitle, engineHolderWithBlankAndTitle),
      List(eBlankWithTitle), Child).
      expected(s"\n<li><a id='Engine1FromTests_${eBlankWithTitle.textOrder}' href='RootUrl/EngineHolder222/EBlankTitle.Engine1FromTests.html'>EBlankTitle</a></li>\n").
      matchOn { case (RenderContext(urlMap, _, _), (engine: Engine) :: _, Child) => s"\n<li><a id='${UrlMap.urlId(engine)}' href='${urlMap(engine)}'>${engine.titleString}</a></li>\n" }.
      build

  val engineReport = builderWithReport("Single Engine report").
    useCase("An engine from tests has a div, a ").
    scenario(context(eBlankWithTitle), List(eBlankWithTitle), Start).
    expected("\n" +
      s"<div class='engineWithTests'><div class='engineSummary'>\n" +
      s"<div class='engineText'>${titleAndIcon(context(eBlankWithTitle), eBlankWithTitle)}</div> <!-- engineText -->\n" +
      s"<div class='usecaseSummary'>\n").
    matchOn {
      case (rc @ RenderContext(urlMap, _, _), (engine: Engine) :: _, Start) => "\n" +
        s"<div class='engineWithTests'><div class='engineSummary'>\n" +
        s"<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n" +
        s"<div class='usecaseSummary'>\n"
    }.

    scenario(context(eBlankWithTitle), List(eBlankWithTitle), End).
    expected("\n</div> <!-- usecaseSummary --></div> <!-- engineWithTests-->\n").
    because { case (RenderContext(urlMap, _, _), (engine: Engine) :: _, End) => true; case _ => false }.
    build

  def main(args: Array[String]) {
    println(ReportDetails())
    val html = documentsAndEngine("Some title", List(eBlankWithTitle), new Date)
    println("------------------Start----------------------")
    println(html)
    println("------------------End----------------------")
  }

}