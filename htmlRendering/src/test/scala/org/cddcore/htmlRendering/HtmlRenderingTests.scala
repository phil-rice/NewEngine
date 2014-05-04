package org.cddcore.htmlRendering

import org.cddcore.cddjunit.CddContinuousIntegrationTest
import org.cddcore.cddjunit.CddContinuousIntegrationRunner
import org.junit.runner.RunWith

@RunWith(classOf[CddContinuousIntegrationRunner])
class HtmlRenderingTests extends CddContinuousIntegrationTest {

//  val engines = List(HtmlRenderer.engineReport)
    val engines = List(HtmlRenderer.titleAndIcon,HtmlRenderer.icon, HtmlRenderer.engineAndDocumentsSingleItemRenderer, HtmlRenderer.engineReport)
}