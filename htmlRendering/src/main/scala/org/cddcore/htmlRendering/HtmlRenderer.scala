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

object SampleContexts {

  val testDate = new Date(2000, 1, 1)
  val rootUrl = "RootUrl"
  val emptyUrlMap = UrlMap(rootUrl)
  //  def documentAndEngineReport(e: Engine) = {
  //    import ReportableHelper._
  //    import EngineTools._
  //    val ed = e.asRequirement
  //    val dAndE = DocumentAndEngineReport(testDate, List(e))
  //    (Report(Some("reportTitle"), nodes = List(dAndE)), dAndE)
  //
  //  }
  def context(rs: Reportable*) = {
    def add[RU <: ReportableToUrl[RU]](urlMap: ReportableToUrl[RU], rs: Traversable[Reportable]) = Lists.decreasingList(rs.toList).foldLeft(urlMap) { _ + _ }
    val urlMap = add(emptyUrlMap, rs)
    import EngineTools._

    val urlMapWithEds = add(emptyUrlMap, rs.map { case e: Engine => e.asRequirement; case r => r })
    val c = RenderContext(urlMapWithEds, testDate)
    c
  }

  def documentContext(d: Document, ds: Document*): (RenderContext, List[Reportable]) = {
    val e = (d :: ds.toList).foldLeft(Engine[Int, Int]())((builder, doc) => builder.reference("", doc)).build
    val report = Report.documentAndEngineReport(Some("documentAndEngineReportTitle"), testDate, List(e))
    (RenderContext(emptyUrlMap ++ report.urlMapPaths, testDate), List(d, report.documentHolder, report))
  }
  val doc1 = Document(title = Some("doc1title"), url = Some("doc1Url"))
  val docWithoutTitle = Document(url = Some("doc1Url"))
  val docHolderWithDoc1 = DocumentHolder(List(doc1), textOrder = 333)

  val eBlank = Engine[Int, Int]().build
  val eBlankD = eBlank.asRequirement
  val engineHolderWithBlank = EngineHolder(List(eBlank), textOrder = 111)

  val eBlankWithTitle = Engine[Int, Int]().title("EBlankTitle").build
  val eBlankWithTitleAndDoc1 = Engine[Int, Int]().title("EBlankTitle").reference("", doc1).build
  val engineHolderWithBlankAndTitle = EngineHolder(List(eBlankWithTitle), textOrder = 222)

  val reqNoTitle = RequirementForTest(textOrder = 666)
  val reqWithTitle = RequirementForTest(title = Some("ReqTitle"))

  val documentAndEngineReport = Report.documentAndEngineReport(Some("documentAndEngineReportTitle"), testDate, List(eBlankWithTitleAndDoc1))

  val eWithUsecasesAndScenarios = Engine[Int, Int].title("eWithUsecasesAndScenarios").
    code((x) => x).
    useCase("useCase0", "useCase0Description").scenario(0).expected(0).
    useCase("useCase1").scenario(1).expected(1).scenario(2).expected(2).
    build
  val ed = eWithUsecasesAndScenarios.asRequirement
  import ReportableHelper._
  val uc0 = ed.useCases(0)
  val uc1 = ed.useCases(1)
  val uc0s0 = ed.scenarios(0)
  val uc1s1 = ed.scenarios(1)
  val uc1s2 = ed.scenarios(2)

  val engineReport = Report.engineReport(Some("engineReportTitle"), testDate, eWithUsecasesAndScenarios)
}

@RunWith(classOf[CddJunitRunner])
object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._

  private def formatter = new SimpleDateFormat("dd MMM yyyy")

  type PathAndTag = (List[Reportable], StartChildEndType)

  import TemplateLike._
  def builderWithReport(title: String) = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title(title).
    useCase("Reports have a huge template at the start, and end. The report title and date are substituted in").
    scenario(context(), List(Report(title = Some("report title"), date = testDate)), Start).
    expected(ReportDetails().reportStart("report title", testDate)).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, Start) => reportDetails.reportStart(r.titleString, date) }.

    scenario(context(), List(Report(title = Some("report title"), date = testDate)), End).
    expected(ReportDetails().reportEnd).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, End) => reportDetails.reportEnd }

  val icon = Engine[Reportable, String]().title("icon").description("returns the html for an image for the icon for the scenario").
    code((_) => "<!-- no icon -->").
    useCase("Engine from tests have icon and title equal to engine titleString").
    scenario(eBlankWithTitle).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (e: EngineFromTests[_, _, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${e.titleString}' />" }.

    useCase("Engine Descriptions have icon and title equal to engine titleString").
    scenario(eBlankWithTitle.asRequirement).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (e: EngineDescription[_, _, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${e.titleString}' />" }.

    useCase("Usescase  have icon and title equal to titleString").
    scenario(uc0).expected("<img	src='http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png' alt='usecase icon' title='useCase0'/>").
    matchOn { case (u: UseCase[_, _, _, _]) => s"<img	src='http://i782.photobucket.com/albums/yy108/phil-rice/useCase_zps23a7250c.png' alt='usecase icon' title='${u.titleString}'/>" }.

    useCase("Scenarios  have icon and title equal to parameters").
    scenario(uc0s0).expected("<img height='15' width='15' src='http://www.constraintdrivendevelopment.org/mediawiki/images/7/73/Scenario.png' alt='scenario icon' title='0'/>").
    matchOn {
      case (s: Scenario[_, _, _, _]) =>
        s"<img height='15' width='15' src='http://www.constraintdrivendevelopment.org/mediawiki/images/7/73/Scenario.png' alt='scenario icon' title='${s.params}'/>"
    }.
    build

  println("Icon:\n" + icon)

  val titleAndIcon = Engine[RenderContext, Reportable, String]().title("titleAndIcon").description("Finds a suitable titleAndIcon for a reportable. Includes links to go to item, and the id from the urlmap").
    useCase("Items that are requirements with titles use their titles").
    scenario(context(reqWithTitle), reqWithTitle).
    expected(s"<a id='RequirementForTest_${reqWithTitle.textOrder}' href='RootUrl/ReqTitle.RequirementForTest.html'>ReqTitle<!-- no icon --></a>").
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) if r.title.isDefined => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${r.titleString}${icon(r)}</a>" }.

    useCase("Items that are requirements without titles are given template name and text order").
    scenario(context(docWithoutTitle), docWithoutTitle).
    expected { val d = s"Document_${docWithoutTitle.textOrder}"; s"<a id='$d' href='RootUrl/Document${docWithoutTitle.textOrder}.Document.html'>$d${icon(docWithoutTitle)}</a>" }.
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${UrlMap.urlId(r)}${icon(r)}</a>" }.

    useCase("Engines are displayed based on their requirements. Without a name uses template name and text order").
    scenario(context(eBlankWithTitle), eBlankWithTitle).
    expected {
      val ed = eBlankWithTitle.asRequirement
      val edTextOrder = ed.textOrder
      s"<a id='EngineDescription_$edTextOrder' href='RootUrl/EBlankTitle.EngineDescription.html'>EBlankTitle${icon(eBlankWithTitle)}</a>"
    }.
    matchOn {
      case (RenderContext(urlMap, _, _), e: Engine) => {
        val ed = e.asInstanceOf[EngineTools[_, _, _, _]].asRequirement
        s"<a id='${UrlMap.urlId(ed)}' href='${urlMap(ed)}'>${ed.titleString}${icon(e)}</a>"
      }
    }.

    build

  val engineAndDocumentsSingleItemRenderer = builderWithReport("Engine and Documents Single Item Renderer").

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
      context(eBlankWithTitle, engineHolderWithBlankAndTitle), List(eBlankWithTitle), Child).
      expected(s"\n<li>${titleAndIcon(context(eBlankWithTitle, engineHolderWithBlankAndTitle), eBlankWithTitle)}</li>\n").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (engine: Engine) :: _, Child) => s"\n<li>${titleAndIcon(rc, engine)}</li>\n" }.
      build

  val engineReport =
    builderWithReport("Single Engine report").
      useCase("An engine from tests has a div, a ").
      scenario(context(eBlankWithTitle), List(eBlankWithTitle), Start).
      expected("\n" +
        s"<div class='engineWithTests'><div class='engineSummary'>\n" +
        s"<div class='engineText'>${titleAndIcon(context(eBlankWithTitle), eBlankWithTitle)}</div> <!-- engineText -->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), (engine: Engine) :: _, Start) => "\n" +
          s"<div class='engineWithTests'><div class='engineSummary'>\n" +
          s"<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n"
      }.

      scenario(context(eBlankWithTitle), List(eBlankWithTitle), Child).
      expected("\n" +
        s"<div class='engineWithTests'><div class='engineSummary'>\n" +
        s"<div class='engineText'>${titleAndIcon(context(eBlankWithTitle), eBlankWithTitle)}</div> <!-- engineText --></div> <!-- engineWithTests-->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), (engine: Engine) :: _, Child) => "\n" +
          s"<div class='engineWithTests'><div class='engineSummary'>\n" +
          s"<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText --></div> <!-- engineWithTests-->\n"
      }.

      scenario(context(eBlankWithTitle), List(eBlankWithTitle), End).
      expected("\n</div> <!-- engineWithTests-->\n").
      because { case (RenderContext(urlMap, _, _), (engine: Engine) :: _, End) => true; case _ => false }.

      useCase("a use case is a header with the title and icon in it, The scenarios are part of the header, and then the description").
      scenario(context(uc0), List(uc0, ed), Start).
      expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(context(uc0), uc0)}</h4>").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, Start) => s"\n<div class='usecaseSummary'><h4>${titleAndIcon(rc, uc)}</h4>" }.

      scenario(context(uc0), List(uc0, ed), Child).
      expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(context(uc0), uc0)}</h4></div> <!-- 'usecaseSummary -->\n").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, Child) => s"\n<div class='usecaseSummary'><h4>${titleAndIcon(rc, uc)}</h4></div> <!-- 'usecaseSummary -->\n" }.

      scenario(context(uc0), List(uc0, ed), End).
      expected("\n</div> <!-- usecaseSummary -->\n").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, End) => "\n</div> <!-- usecaseSummary -->\n" }.

      build

  def main(args: Array[String]) {
    println(ReportDetails())
    val html = Report.html(Report.documentAndEngineReport(Some("Some title"), new Date, List(eBlankWithTitle)), engineAndDocumentsSingleItemRenderer)
    println("------------------Start----------------------")
    println(html)
    println("------------------End----------------------")
  }

}