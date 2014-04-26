package org.cddcore.utilities

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsTest extends AbstractTest {

  "The increasingList method" should "return a gradually increasing list of the items in the original list" in {
    assertEquals(List(), Lists.increasingList(List()))
    assertEquals(List(List("a")), Lists.increasingList(List("a")))
    assertEquals(List(List("a"), List("a", "b")), Lists.increasingList(List("a", "b")))
    assertEquals(List(List("a"), List("a", "b"), List("a", "b", "c")), Lists.increasingList(List("a", "b", "c")))
  }
  "The decreasingList method" should "return a gradually decreasing list of the items in the original list" in {
    assertEquals(List(List("a")), Lists.decreasingList(List("a")))
    assertEquals(List(List("a", "b"), List("b")), Lists.decreasingList(List("a", "b")))
    assertEquals(List(List("a", "b", "c"), List("b", "c"), List("c")), Lists.decreasingList(List("a", "b", "c")))
    assertEquals(List(), Lists.decreasingList(List()))
  }

}