package org.cddcore.htmlRendering

import org.cddcore.engine._
import org.cddcore.utilities._
import org.cddcore.engine.builder._
import scala.xml.Elem
import org.junit.runner.RunWith
import org.cddcore.cddjunit.CddJunitRunner
import StartChildEndType._
import java.util.Date

object HtmlStrings {
  def report = ???
}

case class RenderContext(urlMap: UrlMap, reportDate: String)

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
}
object SampleContexts {

  val testDate = "TestDate"
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

  val doc1 = Document(title = Some("doc1title"), url = Some("doc1Url"))
  val docHolderWithDoc1 = DocumentHolder(List(doc1), textOrder = 333)

}
@RunWith(classOf[CddJunitRunner])
object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._

  type PathAndTag = (List[Reportable], StartChildEndType)
  def documentsAndEngine(title: String, engines: List[Engine], date: String) = {
    val dAndE = DocumentAndEngineHolder(engines)
    val report = Report(Some(title), nodes = List(dAndE))
    val urlMap = UrlMap() ++ report
    val renderContext = RenderContext(urlMap, date)
    val html = Lists.pathToStartChildEnd(report.pathsIncludingSelf).foldLeft("") { case (acc, (path, tag)) => acc + engineAndDocumentsSingleItemRenderer(renderContext, path, tag) }
  }
  import TemplateLike._
  val engineAndDocumentsSingleItemRenderer = Engine[RenderContext, List[Reportable], StartChildEndType, String]().
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

    useCase("Documents are in an anchor").
    scenario(context(doc1, docHolderWithDoc1), List(doc1), Child).
    expected(s"\n<li><a id='Document_${doc1.textOrder}' href='RootUrl/DocumentHolder333/doc1title.Document.html'>\n").
    matchOn {
      case (RenderContext(urlMap, _), ((doc: Document) :: _), Child) =>
        s"\n<li><a id='${UrlMap.urlId(doc)}' href='${urlMap(doc)}'>\n"
    }.

    useCase("Engines are in an anchor").
    scenario(
      context(eBlankWithTitle, engineHolderWithBlankAndTitle),
      List(eBlankWithTitle), Child).
      expected(s"\n<li><a id='Engine1FromTests_${eBlankWithTitle.textOrder}' href='RootUrl/EngineHolder222/EBlankTitle.Engine1FromTests.html'></a></li>\n").
      matchOn {
        case (RenderContext(urlMap, _), (engine: Engine) :: _, Child) =>
          s"\n<li><a id='${UrlMap.urlId(engine)}' href='${urlMap(engine)}'></a></li>\n"
      }.
      build

  println("=====================================================")
  println(engineAndDocumentsSingleItemRenderer)
  println("=====================================================")

  //    expected("<li><a id='Document_CR24' href='file:///C:\Users\Phil\.cdd\Junit\CR24.Document.html' title=''>CR24</a><a href='http://en.wikipedia.org/wiki/Tennis_score'><img src='http://imageshack.com/a/img850/2134/u5hr.png' /></a></li").
}