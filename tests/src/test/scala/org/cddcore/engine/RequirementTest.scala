package org.cddcore.engine

import scala.language.implicitConversions
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.utilities.CodeHolder

case class RequirementForTest(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Requirement.count.getAndIncrement()) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new RequirementForTest(title, description, priority, references, textOrder)
}

case class BuilderNodeForTest[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Exception, R]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val textOrder: Int = Requirement.count.getAndIncrement()) extends BuilderNode[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeForTest(title, description, priority, references, expected, code)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new BuilderNodeForTest[R, RFn](title, description, priority, references, expected, code, textOrder)
}

case class BuilderNodeHolderForTest[R, RFn](nodes: List[BuilderNode[R, RFn]] = List()) extends BuilderNodeHolder[R, RFn] {
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) = new BuilderNodeHolderForTest[R, RFn](nodes)
}
case class BuilderNodeAndHolderForTest[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val expected: Option[Either[Exception, R]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val nodes: List[BuilderNode[R, RFn]],
  val textOrder: Int = 0) extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeAndHolderForTest(title, description, priority, references, expected, code, nodes, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new BuilderNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes, textOrder)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new BuilderNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes, textOrder)

}

class RequirementTest

@RunWith(classOf[JUnitRunner])
class EngineHolderTest extends AbstractTest {
  implicit def toSome[X](x: X) = Some(x)
  type EN = BuilderNodeForTest[String, (String) => String]
  type ENH = BuilderNodeAndHolderForTest[String, (String) => String]
  val en1: EN = BuilderNodeForTest(title = "1")
  val en2: EN = BuilderNodeForTest(title = "2")
  val holderEn1: ENH = BuilderNodeAndHolderForTest(nodes = List(en1))
  val holderEn12: ENH = BuilderNodeAndHolderForTest(nodes = List(en1, en2))
  val holderHolderEn12: ENH = BuilderNodeAndHolderForTest(nodes = List[BuilderNode[String, (String) => String]](
    BuilderNodeAndHolderForTest(nodes = List(en1, en2))))

  "An engine holder foreach  method" should "return all the engine nodes" in {
    assertEquals(List(en1), holderEn1.toList)
    assertEquals(List(en1, en2), holderEn12.toList)
    assertEquals(List(holderEn12, en1, en2), holderHolderEn12.toList)
  }
  "An engine holder all method" should "return all the engine nodes of the requested class" in {
    assertEquals(List(en1), holderEn1.all(classOf[EN]))
    assertEquals(List(en1), holderEn1.all(classOf[BuilderNode[String, (String) => String]]))
    assertEquals(List(en1, en2), holderEn12.all(classOf[EN]))
    assertEquals(List(holderEn12, en1, en2), holderHolderEn12.all(classOf[BuilderNode[String, (String) => String]]))
    assertEquals(List(en1, en2), holderHolderEn12.all(classOf[EN]))
  }

  "An engine holders paths method" should "return all the nodes in a path" in {
    assertEquals(List(
      List(en1)), holderEn1.paths.toList)
    assertEquals(List(
      List(holderEn12),
      List(en1, holderEn12),
      List(en2, holderEn12)),
      holderHolderEn12.paths.toList)
  }
}

