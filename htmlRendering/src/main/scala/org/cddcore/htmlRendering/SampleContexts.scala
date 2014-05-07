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
  implicit def toContext(report: Report) = context(report)

  val testDate = new Date(2000, 1, 1)
  val rootUrl = "RootUrl"
  val emptyUrlMap = UrlMap(rootUrl)

  def context(report: Report) = {
    val urlMap = emptyUrlMap ++ report.urlMapPaths
    val rc = RenderContext(urlMap, SampleContexts.testDate)
    rc
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

  val folding = Engine.foldList[Int, Int].title("Folding Engine Title").
    childEngine("ce0").scenario(0).expected(0).
    childEngine("ce1").scenario(1).expected(2).code { (x) => x * 2 }.because { (x) => x > 0 }.
    scenario(2).expected(4).
    build
    
    
    
  val foldingAsFE = folding.asInstanceOf[FoldingEngine[_, _, _, _, _]]
  val foldingED = folding.asRequirement.asInstanceOf[FoldingEngineDescription[Int, (Int) => Boolean, Int, (Int) => Int, List[Int]]]
  val foldingEngineReport = Report.engineReport(Some("engineReportTitle"), testDate, folding)
  val ce0ED = foldingED.all(classOf[EngineDescription[_, _, _, _]])(0);
  val ce1ED = foldingED.all(classOf[EngineDescription[_, _, _, _]])(1);
  val ce0s0 = foldingED.scenarios(0)
  val ce1s1 = foldingED.scenarios(1)
  val ce1s2 = foldingED.scenarios(2)
  val ce0Tree = foldingAsFE.engines(0).tree
  val ce1Tree = foldingAsFE.engines(1).tree
  val concCe0 = ce0Tree.root
  val decisionCe1 = ce1Tree.root.asInstanceOf[Decision[_, _, _, _]]
  val concYesCe1 = decisionCe1.yes
  val concNoCe1 = decisionCe1.no

  val reqNoTitle = RequirementForTest(textOrder = 666)
  val reqWithTitle = RequirementForTest(title = Some("ReqTitle"))

  val reqWithTitleReport = Report.apply(Some("ReportTitle"), testDate, None, List(reqWithTitle))

}
