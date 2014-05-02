package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine._
import java.util.Date


case class Report(
  val title: Option[String] = None,
  val date: Date = new Date(),
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val nodes: List[Reportable] = List(),
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement with NestedHolder[Reportable] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Report(title,date, description, priority, nodes, references, textOrder)
  override def toString = s"Report(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case p: Report => Requirement.areRequirementFieldsEqual(this, p)
    case _ => false
  }
}


/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}