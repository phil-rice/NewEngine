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

object SampleContexts {
  import scala.language.implicitConversions
  implicit def toPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String]) = new BuilderPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String])
  implicit def toContext(report: Report) = context(report)

  val testDate = new Date(2000, 1, 1)
  val rootUrl = "RootUrl"
  val emptyUrlMap = UrlMap(rootUrl)
  
  def context(report: Report) = {
    val urlMap = emptyUrlMap ++ report.urlMapPaths
    val rc = RenderContext(urlMap, SampleContexts.testDate)
    rc
  }
  class BuilderPimper(builder: Builder3[RenderContext, List[Reportable], StartChildEndType, String, String]) {
    def scenario(report: Report, item: Reportable, sce: StartChildEndType) = {
      val reportPaths = report.reportPaths
      val path = reportPaths.find(_.head == item) match {
        case Some(p) => p
        case _ =>
          throw new IllegalArgumentException(s"\nReport: $report\nLast: $item\n${reportPaths.mkString("\n")}")
      }
      builder.scenario(report, path, sce)
    }
  }

  val doc1 = Document(title = Some("doc1title"), url = Some("doc1Url"))
  val doc1Report = Report(Some("doc1Report"), testDate, None, List(doc1))

  val docNoTitle = Document(url = Some("doc1Url"))
  val doc1NoTitlereport = Report(Some("doc1Report"), testDate, None, List(docNoTitle))

  val eBlank = Engine[Int, Int]().build
  val eBlankED = eBlank.asRequirement
  val eBlankReport = Report.engineReport(Some("engineReportTitle"), testDate, eBlank)

  val eBlankTitle = Engine[Int, Int]().title("EBlankTitle").build
  val eBlankTitleED = eBlankTitle.asRequirement
  val eBlankTitleReport = Report.engineReport(Some("engineReportTitle"), testDate, eBlankTitle)

  val eBlankTitleDoc1 = Engine[Int, Int]().title("EBlankTitle").reference("", doc1).build
  val eBlankTitleDoc1ED = eBlankTitleDoc1.asRequirement
  val eBlankTitleDoc1_DocAndEngineReport = Report.documentAndEngineReport(Some("documentAndEngineReportTitle"), testDate, List(eBlankTitleDoc1))
  val eBlankTitleDoc1_documentHolder = eBlankTitleDoc1_DocAndEngineReport.documentHolder
  val eBlankTitleDoc1_engineHolder = eBlankTitleDoc1_DocAndEngineReport.engineHolder

  val eWithUsecasesAndScenarios = Engine[Int, Int].title("eWithUsecasesAndScenarios").
    useCase("useCase0", "useCase0Description").scenario(0).expected(0).
    useCase("useCase1").scenario(1).expected(2).code { (x) => x * 2 }.because { (x) => x > 0 }.
    scenario(2).expected(4).
    build
  val eWithUsecasesAndScenariosEd = eWithUsecasesAndScenarios.asRequirement
  val engineReport = Report.engineReport(Some("engineReportTitle"), testDate, eWithUsecasesAndScenarios)
  import ReportableHelper._

  val uc0 = eWithUsecasesAndScenariosEd.useCases(0)
  val uc1 = eWithUsecasesAndScenariosEd.useCases(1)
  val uc0s0 = eWithUsecasesAndScenariosEd.scenarios(0)
  val uc1s1 = eWithUsecasesAndScenariosEd.scenarios(1)
  val uc1s2 = eWithUsecasesAndScenariosEd.scenarios(2)

  val tree = eWithUsecasesAndScenarios.asInstanceOf[Engine1FromTests[Int, Int]].tree
  val decision = tree.root.asDecision
  val conclusionYes = decision.yes.asConclusion
  val conclusionNo = decision.no.asConclusion

  val reqNoTitle = RequirementForTest(textOrder = 666)
  val reqWithTitle = RequirementForTest(title = Some("ReqTitle"))

  val reqWithTitleReport = Report.apply(Some("ReportTitle"), testDate, None, List(reqWithTitle))

}
