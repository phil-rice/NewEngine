package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

case class RequirementForTest(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set()) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new RequirementForTest(title, description, priority, references)
}

case class EngineNodeForTest[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Class[_ <: Exception], R]] = None,
  val code: Option[CodeHolder[RFn]] = None) extends EngineNode[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineNodeForTest(title, description, priority, references, expected, code)
  def copyEngineNode(expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new EngineNodeForTest[R, RFn](title, description, priority, references, expected, code)
}

case class EngineNodeHolderForTest[R, RFn](nodes: List[EngineNode[R, RFn]] = List()) extends EngineNodeHolder[R, RFn] {
  def copyNodes(nodes: List[EngineNode[R, RFn]]) = new EngineNodeHolderForTest[R, RFn](nodes)
}
case class EngineNodeAndHolderForTest[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Class[_ <: Exception], R]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val nodes: List[EngineNode[R, RFn]]) extends EngineNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineNodeAndHolderForTest(title, description, priority, references, expected, code, nodes)
  def copyEngineNode(expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new EngineNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes)
  def copyNodes(nodes: List[EngineNode[R, RFn]]) =
    new EngineNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes)

}

class RequirementTest

@RunWith(classOf[JUnitRunner])
class EngineHolderTest extends AbstractTest {
  implicit def toSome[X](x: X) = Some(x)
  type EN = EngineNodeForTest[String, (String) => String]
  type ENH = EngineNodeAndHolderForTest[String, (String) => String]
  val en1: EN = EngineNodeForTest(title = "1")
  val en2: EN = EngineNodeForTest(title = "2")
  val holderEn1: ENH = EngineNodeAndHolderForTest(nodes = List(en1))
  val holderEn12: ENH = EngineNodeAndHolderForTest(nodes = List(en1, en2))
  val holderHolderEn12: ENH = EngineNodeAndHolderForTest(nodes = List[EngineNode[String, (String) => String]](
    EngineNodeAndHolderForTest(nodes = List(en1, en2))))

  "An engine holder foreach  method" should "return all the engine nodes" in {
    assertEquals(List(en1), holderEn1.toList)
    assertEquals(List(en1, en2), holderEn12.toList)
    assertEquals(List(holderEn12, en1, en2), holderHolderEn12.toList)
  }
  "An engine holder all method" should "return all the engine nodes of the requested class" in {
    assertEquals(List(en1), holderEn1.all(classOf[EN]))
    assertEquals(List(en1), holderEn1.all(classOf[EngineNode[String, (String) => String]]))
    assertEquals(List(en1, en2), holderEn12.all(classOf[EN]))
    assertEquals(List(holderEn12, en1, en2), holderHolderEn12.all(classOf[EngineNode[String, (String) => String]]))
    assertEquals(List(en1, en2), holderHolderEn12.all(classOf[EN]))
  }

  "An engine holders paths method" should "return all the nodes in a path" in {
    assertEquals(List(
      List(holderEn1),
      List(en1, holderEn1)), holderEn1.paths.toList)
    assertEquals(List(
      List(holderHolderEn12),
      List(holderEn12, holderHolderEn12),
      List(en1, holderEn12, holderHolderEn12),
      List(en2, holderEn12, holderHolderEn12)),
      holderHolderEn12.paths.toList)
  }
}

