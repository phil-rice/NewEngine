package org.cddcore.htmlRendering

import org.cddcore.engine.AbstractTest
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.cddcore.engine.SomeHoldersForTest
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class UrlMapTest extends AbstractTest with SomeHoldersForTest {

  val emptyMap = new UrlMap()

  def checkNoSuchElementException(e: RuntimeException) {
    assert { e.getCause.isInstanceOf[NoSuchElementException] }
  }
  "An empty UrlMap" should "throw exceptions or return none if things aren't found in it" in {
    checkNoSuchElementException(evaluating { emptyMap(en1) } should produce[RuntimeException])
    assertEquals(None, emptyMap.get(en1))
    evaluating { emptyMap("") } should produce[NoSuchElementException]
    assertEquals(None, emptyMap.get(""))
  }

  "A UrlMap" should "return the url and for a reportable that is at the head of a path with one item in it" in {
    val urlMap = emptyMap + (List(en1) -> "urlEn1")
    assertEquals(List(en1), urlMap("urlEn1"))
    assertEquals("urlEn1", urlMap(en1))
  }
  it should "return the url and for a reportable that is at the head of a path with multiple items in it" in {
    val urlMap = emptyMap + (List(en1, holderEn1) -> "urlEn1")
    assertEquals(List(en1, holderEn1), urlMap("urlEn1"))
    assertEquals("urlEn1", urlMap(en1))
  }

  it should "return true if the reportable passed to contains has been seen" in {
    val urlMap = emptyMap + (List(en1, holderEn1) -> "urlEn1")
    assertEquals(true, urlMap.contains(en1))
    assertEquals(false, urlMap.contains(en2))
    assertEquals(false, urlMap.contains(holderEn1))
  }

}