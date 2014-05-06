package org.cddcore.htmlRendering

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem
import org.cddcore.cddjunit.CddJunitRunner
import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.cddcore.utilities._
import org.cddcore.utilities.StartChildEndType._
import org.junit.runner.RunWith
import java.io.File

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
case class EngineHolder(val engines: List[Engine], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable {
  import EngineTools._
  val nodes = engines.map(_.asRequirement)
}

@RunWith(classOf[CddJunitRunner])
object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._

  private def formatter = new SimpleDateFormat("dd MMM yyyy")

  type PathAndTag = (List[Reportable], StartChildEndType)

  import TemplateLike._
  def builderWithReport(title: String) = Engine[RenderContext, List[Reportable], StartChildEndType, String]().title(title).
    useCase("Reports have a huge template at the start, and end. The report title and date are substituted in").
    scenario(engineReport, engineReport, Start).
    expected(ReportDetails().reportStart("engineReportTitle", testDate)).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, Start) => reportDetails.reportStart(r.titleString, date) }.

    scenario(engineReport, engineReport, End).
    expected(ReportDetails().reportEnd).
    matchOn { case (RenderContext(_, date, reportDetails), (r: Report) :: _, End) => reportDetails.reportEnd }

  val icon = Engine[Reportable, String]().title("icon").description("returns the html for an image for the icon for the scenario").
    code((_) => "<!-- no icon -->").
    useCase("Engine from tests have icon and title equal to engine titleString").
    scenario(eBlankTitle).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (e: EngineFromTests[_, _, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${e.titleString}' />" }.

    useCase("Engine Descriptions have icon and title equal to engine titleString").
    scenario(eBlankTitleED).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='EBlankTitle' />").
    matchOn { case (e: EngineDescription[_, _, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engine_zps9a86cef4.png' alt='engine with tests icon' title='${e.titleString}' />" }.

    useCase("Folding Engine Descriptions have icon and title equal to engine titleString").
    scenario(foldingED).expected("<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png' alt='folding engine icon' title='Folding Engine Title' />").
    matchOn { case (e: FoldingEngineDescription[_, _, _, _, _]) => s"<img src='http://i782.photobucket.com/albums/yy108/phil-rice/engineFold2_zpsb62930b9.png' alt='folding engine icon' title='${e.titleString}' />" }.

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

  val linkAndIcon = Engine[RenderContext, Reportable, String]().title("linkAndIcon").description("Displays a suitable icon in a link for the reportable").
    code { case (rc @ RenderContext(urlMap, _, _), r) => s"<a href='${urlMap(r)}'>${icon(r)}</a>" }.
    scenario(context(reqWithTitleReport), reqWithTitle).
    expected(s"<a href='RootUrl/ReportTitle/ReqTitle.RequirementForTest.html'>${icon(reqWithTitle)}</a>").
    build

  val titleAndIcon = Engine[RenderContext, Reportable, String]().title("titleAndIcon").description("Finds a suitable titleAndIcon for a reportable. Includes links to go to item, and the id from the urlmap").
    useCase("Items that are requirements with titles use their titles").
    scenario(context(reqWithTitleReport), reqWithTitle).
    expected(s"<a id='RequirementForTest_${reqWithTitle.textOrder}' href='RootUrl/ReportTitle/ReqTitle.RequirementForTest.html'>ReqTitle<!-- no icon --></a>").
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) if r.title.isDefined => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${r.titleString}${icon(r)}</a>" }.

    useCase("Items that are requirements without titles are given template name and text order").
    scenario(context(doc1NoTitlereport), docNoTitle).
    expected { val d = s"Document_${docNoTitle.textOrder}"; s"<a id='$d' href='RootUrl/doc1Report/Document${docNoTitle.textOrder}.Document.html'>$d${icon(docNoTitle)}</a>" }.
    matchOn { case (RenderContext(urlMap, _, _), r: Requirement) => s"<a id='${UrlMap.urlId(r)}' href='${urlMap(r)}'>${UrlMap.urlId(r)}${icon(r)}</a>" }.

    useCase("Engines are displayed based on their requirements. Without a name uses template name and text order").
    scenario(eBlankTitleReport, eBlankTitleED).
    expected { s"<a id='EngineDescription_${eBlankTitleED.textOrder}' href='RootUrl/engineReportTitle/EBlankTitle.EngineDescription.html'>EBlankTitle${icon(eBlankTitleED)}</a>" }.
    matchOn { case (RenderContext(urlMap, _, _), ed: EngineDescription[_, _, _, _]) => s"<a id='${UrlMap.urlId(ed)}' href='${urlMap(ed)}'>${ed.titleString}${icon(ed)}</a>" }.

    build

  val engineAndDocumentsSingleItemRenderer = builderWithReport("Engine and Documents Single Item Renderer").
    useCase("Engine Holders have a div, and hold the items as an unorder list").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_DocAndEngineReport.engineHolder, Start).
    expected("\n<div class='engineHolder'><h3>Engines</h3><ul>\n").
    because { case (_, (holder: EngineHolder) :: _, Start) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_engineHolder, End).
    expected("\n</ul></div> <!-- engineHolder -->\n").
    because { case (_, (holder: EngineHolder) :: _, End) => true; case _ => false }.

    useCase("Document Holders have a div, and hold the items as an unorder list").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, Start).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul>\n").
    because { case (_, (holder: DocumentHolder) :: _, Start) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, End).
    expected("\n</ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, End) => true; case _ => false }.

    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1_documentHolder, Child).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul><li>No documents</li></ul></div> <!-- documentHolder -->\n").
    because { case (_, (holder: DocumentHolder) :: _, Child) => true; case _ => false }.

    useCase("Documents are in an anchor, and use the TitleAndIcon engine").
    scenario(eBlankTitleDoc1_DocAndEngineReport, doc1, Child).
    expected(s"\n<li>${titleAndIcon(context(eBlankTitleDoc1_DocAndEngineReport), doc1)}</li>\n").
    matchOn { case (rc @ RenderContext(urlMap, _, _), ((doc: Document) :: _), Child) => s"\n<li>${titleAndIcon(rc, doc)}</li>\n" }.

    useCase("Engines are in an anchor").
    scenario(eBlankTitleDoc1_DocAndEngineReport, eBlankTitleDoc1ED, Child).
    expected(s"\n<li>${titleAndIcon(context(eBlankTitleDoc1_DocAndEngineReport), eBlankTitleDoc1ED)}</li>\n").
    matchOn { case (rc @ RenderContext(urlMap, _, _), (ed: EngineDescription[_, _, _, _]) :: _, Child) => s"\n<li>${titleAndIcon(rc, ed)}</li>\n" }.
    build

  def indent(path: List[Reportable]) = path.indexWhere(_.isInstanceOf[DecisionTree[_, _, _, _]]) match {
    case 1 => ""
    case i => s"<div class='indent'>${List.fill(i - 1)("&#160;").mkString("")}</div>"
  }

  val engineReportSingleItemRenderer =
    builderWithReport("Single Engine report").

      useCase("A folding engine"). 
      scenario(foldingEngineReport, foldingED, Start).
      expected("\n" +
        s"<div class='engineWithChildren'><div class='engineWithChildrenSummary'>" +
        s"<div class='engineText'>${titleAndIcon(foldingEngineReport, foldingED)}</div> <!-- engineText -->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), (engine: FoldingEngineDescription[_, _, _, _, _]) :: _, Start) => "\n" +
          s"<div class='engineWithChildren'><div class='engineWithChildrenSummary'>" +
          s"<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n"
      }.
      scenario(foldingEngineReport, foldingED, End).
      expected("\n</div> <!--engineWithChildrenSummary --></div> <!-- engineWithChildren -->\n").
      because { case (rc @ RenderContext(urlMap, _, _), (engine: FoldingEngineDescription[_, _, _, _, _]) :: _, End) => true; case _ => false }.

      useCase("An engine from tests has a div, a ").
      scenario(engineReport, eWithUsecasesAndScenariosEd, Start).
      expected("\n" +
        s"<div class='engineWithTests'><div class='engineSummary'>\n" +
        s"<div class='engineText'>${titleAndIcon(engineReport, eWithUsecasesAndScenariosEd)}</div> <!-- engineText -->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), (engine: EngineDescription[_, _, _, _]) :: _, Start) => "\n" +
          s"<div class='engineWithTests'><div class='engineSummary'>\n" +
          s"<div class='engineText'>${titleAndIcon(rc, engine)}</div> <!-- engineText -->\n"
      }.

      scenario(engineReport, eWithUsecasesAndScenariosEd, End).
      expected("\n</div> <!-- engineWithTests-->"). //the decision tree closes off the engineSummary div
      because { case (RenderContext(urlMap, _, _), (engine: EngineDescription[_, _, _, _]) :: _, End) => true; case _ => false }.

      useCase("a use case is a header with the title and icon in it, The scenarios are part of the header, and then the description").
      scenario(engineReport, uc0, Start).
      expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(engineReport, uc0)}").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, Start) => s"\n<div class='usecaseSummary'><h4>${titleAndIcon(rc, uc)}" }.

      scenario(engineReport, uc0, Child).
      expected(s"\n<div class='usecaseSummary'><h4>${titleAndIcon(engineReport, uc0)}</h4>useCase0Description</div> <!-- 'usecaseSummary -->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, Child) => s"\n<div class='usecaseSummary'>" +
          s"<h4>${titleAndIcon(rc, uc)}</h4>${uc.description.getOrElse("")}</div> <!-- 'usecaseSummary -->\n"
      }.

      scenario(engineReport, uc0, End).
      expected("\n</h4>useCase0Description</div> <!-- usecaseSummary -->\n").
      matchOn { case (rc @ RenderContext(urlMap, _, _), (uc: UseCase[_, _, _, _]) :: _, End) => s"\n</h4>${uc.description.getOrElse("")}</div> <!-- usecaseSummary -->\n" }.

      useCase("a scenario is just the icon").
      scenario(engineReport, uc0s0, Child).
      expected(icon(uc0s0)).
      matchOn { case (_, (s: Scenario[_, _, _, _]) :: _, Child) => icon(s) }.

      useCase("A decision tree, closes off the engine with summary div and  get's it's own div").
      scenario(engineReport, tree, Start).
      expected("\n</div> <!-- engineSummary'--><div class='decisionTree'>\n").
      because { case (_, (s: DecisionTree[_, _, _, _]) :: _, Start) => true; case _ => false }.

      scenario(engineReport, tree, End).
      expected("\n</div><!-- decisionTree -->\n").
      matchOn { case (_, (s: DecisionTree[_, _, _, _]) :: _, End) => "\n</div><!-- decisionTree -->\n" }.

      useCase("A decision get's it's own div, and a because").
      scenario(engineReport, decision, Start).
      expected("\n<div class='decision'><div class='because'><span class='keyword'>if&#160;</span>" +
        "\n<div class='because'>x.>(0)</div><!-- because --></div><!-- because -->\n").
      matchOn {
        case (_, path @ (d: Decision[_, _, _, _]) :: _, Start) =>
          s"\n<div class='decision'><div class='because'>${indent(path)}<span class='keyword'>if&#160;</span>" +
            s"\n<div class='because'>${d.prettyString}</div><!-- because --></div><!-- because -->\n"
      }.
      scenario(engineReport, decision, End).
      expected("</div><!--decision -->\n").
      matchOn { case (_, (s: Decision[_, _, _, _]) :: _, End) => "</div><!--decision -->\n" }.

      useCase("An elseclause is artificially inserted to allow else to be displayed easily").
      scenario(engineReport, ElseClause(), Child).
      expected("<div class='else'><div class='indent'>&#160;</div><span class='keyword'>else&#160;</span></div>").
      matchOn { case (_, path @ (s: ElseClause) :: _, _) => s"<div class='else'>${indent(path)}<span class='keyword'>else&#160;</span></div>" }.

      useCase("A conclusion get's it's own div").
      scenario(engineReport, conclusionYes, Child).
      expected(s"\n<div class='result'><div class='indent'>&#160;</div><span class='keyword'>then&#160;</span>" +
        s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s1.textOrder}.Scenario.html'>${icon(uc1s1)}</a>" +
        s"<a class='scenarioLink' href='RootUrl/engineReportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s2.textOrder}.Scenario.html'>${icon(uc1s2)}</a>" +
        s"<div class='conclusion'>((x: Int) => x.*(2))</div><!-- conclusion --></div><!-- result -->\n").
      matchOn {
        case (rc @ RenderContext(urlMap, _, _), path @ (c: Conclusion[_, _, _, _]) :: _, Child) => "\n" +
          s"<div class='result'>${indent(path)}<span class='keyword'>then&#160;</span>" +
          s"${c.scenarios.map((s) => s"<a class='scenarioLink' href='${urlMap(s)}'>${icon(s)}</a>").mkString("")}" +
          s"<div class='conclusion'>${c.code.description}</div><!-- conclusion --></div><!-- result -->\n"
      }.

      build

  def main(args: Array[String]) {
    println(ReportDetails())
    println("------------------DocumentAndEngine----------------------")
    println(Report.html(Report.documentAndEngineReport(Some("Some title"), new Date, List(eBlankTitle)), engineAndDocumentsSingleItemRenderer))
    println("------------------SingleEngine----------------------")
    println(Report.html(Report.engineReport(Some("Some title"), new Date, folding), engineReportSingleItemRenderer))
    Files.printToFile(new File("c:/users/phil/desktop/test.html"))(_.println(Report.html(foldingEngineReport, engineReportSingleItemRenderer)))
  }

}