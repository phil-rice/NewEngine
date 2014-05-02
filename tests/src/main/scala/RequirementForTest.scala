package org.cddcore.tests

import org.cddcore.engine._
import org.cddcore.utilities._

case class RequirementForTest(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement {
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
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNode[R, RFn] {
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
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new BuilderNodeAndHolderForTest(title, description, priority, references, expected, code, nodes, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new BuilderNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes, textOrder)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new BuilderNodeAndHolderForTest[R, RFn](title, description, priority, references, expected, code, nodes, textOrder)

}
