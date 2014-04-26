package org.cddcore.htmlRendering

import org.cddcore.engine._
import org.cddcore.utilities._
import org.cddcore.engine.builder._
import scala.xml.Elem
import org.junit.runner.RunWith
import org.cddcore.cddjunit.CddJunitRunner
import StartChildEndType._

object HtmlStrings {
  def report = ???
}

case class RenderContext(urlMap: UrlMap, path: List[Reportable])

object SampleContexts {
  def context(rs: Reportable*): RenderContext = context(rs.toList)

  def context(rs: List[Reportable]): RenderContext = {
    val path = rs.toList
    val result = new SimpleReportableToUrl
    RenderContext(result, path)
  }

  def engineHolder[Params, BFn, R, RFn](es: Engine[_, _, _, _]*) = EngineHolder(es.toList)
  def documentHolder(d: Document*) = DocumentHolder(d.toList)

  def engineContext(e: Engine[_, _, _, _], es: Engine[_, _, _, _]*) = context(e, engineHolder((e :: es.toList): _*))
  def documentContext(d: Document, ds: Document*) = context(d, documentHolder((d :: ds.toList): _*))

  def engineHolderContext(es: Engine[_, _, _, _]*) = context(engineHolder(es: _*))
  def documentHolderContext(ds: Document*) = context(documentHolder(ds: _*))

  val eBlank = Engine[Int, Int]().build
  def eBlank(title: String) = Engine[Int, Int]().title(title).build

  val doc1 = Document(title = Some("doc1title"), url = Some("doc1Url"))
  val docHolderWithDoc1 = documentHolder(Document(title = Some("doc1title")))

}
@RunWith(classOf[CddJunitRunner])
object HtmlRenderer extends DecisionTreeBuilderForTests2[RenderContext, StartChildEndType, Elem] {
  import SampleContexts._

  val engineAndDocumentsRenderer = Engine[RenderContext, StartChildEndType, String]().
    useCase("Engine Holders have a div, and hold the items as an unorder list").
    scenario(engineHolderContext(eBlank), Start).
    expected("\n<div class='engineHolder'><h3>Engines</h3><ul>\n").
    because { case (RenderContext(_, (holder: EngineHolder) :: _), Start) => true; case _ => false }.

    scenario(engineHolderContext(eBlank), End).
    expected("\n</ul></div> <!-- engineHolder -->\n").
    because { case (RenderContext(_, (holder: EngineHolder) :: _), End) => true; case _ => false }.

    useCase("Document Holders have a div, and hold the items as an unorder list").
    scenario(documentHolderContext(doc1), Start).
    expected("\n<div class='documentHolder'><h3>Documents</h3><ul>\n").
    because { case (RenderContext(_, (holder: DocumentHolder) :: _), Start) => true; case _ => false }.

    scenario(documentHolderContext(doc1), End).
    expected("\n</ul></div> <!-- documentHolder -->\n").
    because { case (RenderContext(_, (holder: DocumentHolder) :: _), End) => true; case _ => false }.

    useCase("Documents are in an anchor").
    scenario(documentContext(doc1), Start).
    expected("\n<li> id='Document_doc1title' href=file:///\n").
    matchOn { case (RenderContext(_, (holder: DocumentHolder) :: _), End) => 
      s"\n<li>\n" }.
    build

  //    expected("<li><a id='Document_CR24' href='file:///C:\Users\Phil\.cdd\Junit\CR24.Document.html' title=''>CR24</a><a href='http://en.wikipedia.org/wiki/Tennis_score'><img src='http://imageshack.com/a/img850/2134/u5hr.png' /></a></li").
}