package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine._
import org.cddcore.engine.builder._

import StartChildEndType._
import java.util.Date

object Report {
  def apply(title: Option[String] = None, date: Date = new Date, description: Option[String] = None, nodes: List[Reportable] = List()) =
    new SimpleReport(title, date, description, nodes)
  def documentAndEngineReport(title: Option[String], date: Date, engines: Traversable[Engine], description: Option[String] = None) =
    new DocumentAndEngineReport(title, date, engines, description)
  def engineReport(title: Option[String], date: Date, engine: Engine, description: Option[String] = None) =
    new EngineReport(title, date, engine, description)

  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String]): String = {
    val renderContext = RenderContext(UrlMap() ++ report.urlMapPaths, new Date())
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => html + engine(renderContext, path, cse) }
  }

  def html(report: Report, engine: Function3[RenderContext, List[Reportable], StartChildEndType, String], renderContext: RenderContext): String =
    Lists.traversableToStartChildEnd(report.reportPaths).foldLeft("") { case (html, (path, cse)) => engine(renderContext, path, cse) }
}

trait Report extends Requirement {
  def title: Option[String]
  def date: Date
  def description: Option[String]
  def priority: Option[Int] = None
  def references: Set[Reference] = Set()
  def reportPaths: List[List[Reportable]]
  def urlMapPaths: List[List[Reportable]] = reportPaths
}

case class SimpleReport(
  val title: Option[String],
  val date: Date,
  val description: Option[String],
  val nodes: List[Reportable],
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  val reportPaths = pathsIncludingSelf.toList

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new SimpleReport(title, date, description, nodes, textOrder)

  override def toString = s"Report(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case p: SimpleReport => Requirement.areRequirementFieldsEqual(this, p) && nodes == p.nodes
    case _ => false
  }
}

case class DocumentAndEngineReport(title: Option[String],
  val date: Date,
  val engines: Traversable[Engine],
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  import EngineTools._
  import ReportableHelper._

  val documents = engines.flatMap(_.asRequirement.documents).toList.removeDuplicates
  val sortedEngines = engines.toList.sortBy(_.textOrder)
  val documentHolder = DocumentHolder(documents)
  val engineHolder = EngineHolder(sortedEngines)
  val nodes = List(documentHolder, engineHolder)
  val reportPaths = pathsIncludingSelf.toList

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new DocumentAndEngineReport(title, date, engines, description, textOrder)

  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case de: DocumentAndEngineReport => Requirement.areRequirementFieldsEqual(this, de) && engines == de.engines
    case _ => false
  }
}

object ElseClause {
  private val instance = new ElseClause
  def apply() = instance
}
class ElseClause(val textOrder: Int = Reportable.nextTextOrder) extends Reportable

case class EngineReport(title: Option[String],
  val date: Date,
  val engine: Engine,
  val description: Option[String] = None,
  val textOrder: Int = Reportable.nextTextOrder) extends Report with NestedHolder[Reportable] {
  import EngineTools._
  import ReportableHelper._

  val ed = engine.asRequirement
  val nodes = List(ed)

  def treePaths[Params, BFn, R, RFn](path: List[Reportable], tree: DecisionTree[Params, BFn, R, RFn]): List[List[Reportable]] = (tree :: path) :: treePaths(tree :: path, tree.root)

  //builds up in reverse order
  private def treePaths[Params, BFn, R, RFn](path: List[Reportable], node: DecisionTreeNode[Params, BFn, R, RFn]): List[List[Reportable]] = {
    node match {
      case c: Conclusion[Params, BFn, R, RFn] => List((c :: path))
      case d: Decision[Params, BFn, R, RFn] => {
        val dPath = (d :: path)
        val yesPaths = treePaths(dPath, d.yes)
        val noPaths = treePaths(dPath, d.no)
        dPath :: yesPaths ::: List(ElseClause() :: dPath) ::: noPaths
      }
    }
  }

  val basePath = List(engine.asRequirement, this)
  val baseReportPaths: List[List[Reportable]] = List(this) :: basePath :: Nil
  val reportPaths: List[List[Reportable]] = engine match {
    case e: EngineFromTests[_, _, _, _] => baseReportPaths ::: ed.pathsFrom(basePath).toList ::: treePaths(basePath, e.tree)
    case f: FoldingEngine[_, _, _, _, _] => baseReportPaths ::: f.engines.flatMap((e) => {
      val ed = e.asRequirement
      ed.pathsIncludingSelf(basePath).toList ::: treePaths(ed :: basePath, e.tree)
    })
  }

  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineReport(title, date, engine, description, textOrder)

  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case er: EngineReport => Requirement.areRequirementFieldsEqual(this, er) && engine == er.engine
    case _ => false
  }
}

/** A reportable with a wrapper is used in the reports when making a projection of an original report. This allows the correct urls to be determined */
trait ReportableWrapper extends Reportable {
  def delegate: Option[Reportable]

}