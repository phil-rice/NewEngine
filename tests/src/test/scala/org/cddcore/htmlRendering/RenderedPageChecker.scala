package org.cddcore.htmlRendering

import scala.xml.Elem
import scala.language.implicitConversions
import scala.xml.NodeSeq
import scala.xml.XML
import scala.xml.Node
import scala.language.postfixOps
import org.cddcore.htmlRendering._
import org.cddcore.engine._
import EngineTools._
import ReportableHelper._
import PathUtils._

trait HtmlChecker extends AssertEquals {
  def html: String
  val xml: Elem = XML.loadString(html)
  val divs = xml \\ "div"

  def divsWith(className: String, divs: NodeSeq = divs) =
    divs filter attributeEquals("class", className)

  def nthNodeWith(parent: NodeSeq, n: Int, className: String) = {
    val result = parent(n)
    assertEquals(className, result.attribute("class").get.text)
    result
  }

  def onlyDivWith(className: String, divs: NodeSeq = divs) = only(divsWith(className, divs))
  def only(nodes: NodeSeq) = {
    assertEquals(1, nodes.size)
    nodes head
  }
  def removeBlankNodes(node: Node) =
    node.child.filter((n) => {
      val len = n.text.trim.length
      len != 0
    })
  def findTdInRowWithTitle(node: Node, title: String) = {
    val tr = findRowWithTitle(node, title)
    tr match {
      case Some(tr) =>
        val tds = tr \ "td"
        assertEquals(2, tds.size)
        val td = tds(1)
        assertEquals("value", td.attribute("class").get.text)
        Some(td)
      case None => None
    }
  }

  def findRowWithTitle(node: Node, title: String) = {
    (node \\ "tr").find((tr) => {
      val td = (tr \ "td")
      val tdWithTitle = td(0)
      val classAttribute = td.head.attribute("class")
      classAttribute match {
        case Some(ca) =>
          assertEquals(classAttribute.get.text, "title")
          tdWithTitle.text.trim == title.trim
        case _ => false
      }
    })
  }

  def attributeEquals(name: String, value: String)(node: Node) = {
    val n = node;
    val attribute = node.attribute(name)
    val filtered = attribute.filter(
      (v) =>
        v.toString == value)
    filtered.isDefined
  }
}

trait HtmlRenderedChecker extends HtmlChecker {
  val renderContext: RenderContext

  val urlMap: UrlMap = renderContext.urlMap
  val ldp = renderContext.loggerDisplayProcessor
  val reportDiv = onlyDivWith("report");

  def checkTopLine(expectedTitle: String) {
    val topLineDiv = onlyDivWith("topLine", reportDiv.child)
    val cddLogoDiv = onlyDivWith("cddLogo", topLineDiv.child)
    val cddBoxDiv = onlyDivWith("cddBox", topLineDiv.child)
    val reportTopBoxDiv = onlyDivWith("reportTopBox", topLineDiv.child)
    val reportTitlesDiv = divsWith("reportTitle", reportTopBoxDiv.child)
    assertTextEquals("Report name", reportTitlesDiv(0))
    assertTextEquals("Report date", reportTitlesDiv(1))
    val reportTextDiv = onlyDivWith("reportText", reportTopBoxDiv.child)
    val reportDateDiv = onlyDivWith("reportDate", reportTopBoxDiv.child)
    assertTextEquals(expectedTitle.trim, reportTextDiv)
  }

  def checkEngineSummaryChecker(path: List[Reportable], engineNode: Node) {
    val engineSummary = onlyDivWith("engineSummary", engineNode.child)
    val engineText = onlyDivWith("engineText", engineSummary.child)
    //    val engineTable = onlyDivWith("engineTable", engineSummary.child)
    val decisionTree = onlyDivWith("decisionTree", engineNode.child)

    val engineLink = (engineText \ "a").head
    val engineLinkText = engineLink.text
    val engineLinkUrl = engineLink.attribute("href").get.text

    val ed = path.head.asInstanceOf[EngineRequirement[_, _, _, _]]
    assertEquals(urlMap(ed), engineLinkUrl)
  }

  def checkUsecaseWithScenariosSummarized(path: List[Reportable], useCaseNode: Node) {
    val useCase = path.head.asInstanceOf[UseCase[_, _, _, _]]
    val h4 = only(useCaseNode \ "h4")
    val links = h4 \ "a"
    assertEquals(1 + useCase.nodes.size, links.size) //the +1 is the use case itself
    val useCaselink = links.head
    assertTextEquals(useCase.titleString, useCaselink)
    assertEquals(urlMap(useCase), useCaselink.attribute("href").get text)

    val scenarioLinks = links.tail
    val summaries = useCase.scenarios.zipAll(scenarioLinks, null, null).map(_ match {
      case (s: Scenario[_, _, _, _], scenarioLink) => {
        assertTextEquals(s.titleString, scenarioLink)
        assertEquals(urlMap(s), scenarioLink.attribute("href").get text)
      }
    })
  }
  def checkUsecaseWithScenariosDetails(path: List[Reportable], useCaseNode: Node) {
    val useCase = path.head.asInstanceOf[UseCase[_, _, _, _]]
    val h4 = only(useCaseNode \ "h4")
    val links = h4 \ "a"
    assertEquals(1 , links.size) 
    val useCaselink = links.head
    assertTextEquals(useCase.titleString, useCaselink)
    assertEquals(urlMap(useCase), useCaselink.attribute("href").get text)

    val scenarioDivs = divsWith( "scenario", useCaseNode.child)
    val summaries = useCase.scenarios.zipAll(scenarioDivs, null, null).map(_ match {
      case (s: Scenario[_, _, _, _], scenarioDiv) => checkScenarioDetails(s :: path, scenarioDiv)
    })
  }

  def checkScenarioDetails(path: List[Reportable], scenarioNode: Node) {
    val test = path.head.asInstanceOf[Scenario[_, _, _, _]]
    val scenarioText = onlyDivWith("scenarioText", scenarioNode.child)
    val linkNode = only(scenarioText \ "a")
    val link = linkNode.attribute("href").get
    assertEquals(urlMap(path.head), link text)
    val table = only(scenarioNode \ "table")
    val trs = table \ "tr"
    val paramTd = findTdInRowWithTitle(table, "Parameter")
    val paramText = paramTd.get.text
    assertEquals(paramText, test.prettyPrintParams)

    val expectedTd = findTdInRowWithTitle(table, "Expected")
    assertTextEquals(test.prettyPrintExpected, expectedTd.get)
  }

}

class SimpleHtmlRenderedChecker(val html: String, val renderContext: RenderContext) extends HtmlRenderedChecker {
  def this(tuple: (String, RenderContext)) = this(tuple._1, tuple._2)

}