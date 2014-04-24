package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions
import org.cddcore.utilities.Maps
import org.cddcore.utilities.CodeHolder
import org.cddcore.utilities.NestedHolder
import org.cddcore.utilities.StartChildEndType
trait Reportable {
  val textOrder: Int
}
trait ReportableWithTemplate extends Reportable {
  val template: String
}

object Reportable {
  private final val count = new AtomicInteger(0)
  def nextTextOrder = count.getAndIncrement()
  def compare[R](left: Either[Exception, R], right: Either[Exception, R]) = {
    (left, right) match {
      case (Left(le), Left(re)) => le.getClass() == re.getClass()
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
  }
}
object Requirement {
  def areRequirementFieldsEqual(r1: Requirement, r2: Requirement) = {
    r1.title == r2.title &&
      r1.description == r2.description &&
      r1.priority == r2.priority &&
      r1.references == r2.references
  }
  def areBuilderNodeFieldsEquals[R, RFn](r1: BuilderNode[R, RFn], r2: BuilderNode[R, RFn]) = {
    areRequirementFieldsEqual(r1, r2) && r1.expected == r2.expected && r1.code == r2.code
  }
  def areBuilderNodeAndHolderFieldsEqual[R, RFn](r1: BuilderNodeAndHolder[R, RFn], r2: BuilderNodeAndHolder[R, RFn]) = {
    Requirement.areBuilderNodeFieldsEquals(r1, r2) && r1.nodes == r2.nodes
  }

}

trait Requirement extends Reportable {
  def title: Option[String]
  def description: Option[String]
  def priority: Option[Int]
  def references: Set[Reference]
  def copyRequirement(title: Option[String] = title,
    description: Option[String] = description,
    priority: Option[Int] = priority,
    references: Set[Reference] = references): Requirement
  lazy val titleString = title.getOrElse("")
  def titleOrDescription(default: String) = title.getOrElse(description.getOrElse(default))
}

trait BuilderNode[R, RFn] extends Requirement {
  def expected: Option[Either[Exception, R]]
  def code: Option[CodeHolder[RFn]]
  def copyBuilderNode(
    expected: Option[Either[Exception, R]] = expected,
    code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn]
}

trait BuilderNodeHolder[R, RFn] extends NestedHolder[BuilderNode[R, RFn]] {
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn]
}

trait FoldingBuilderNodeAndHolder[R, RFn, FullR] extends BuilderNodeAndHolder[R, RFn] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: CodeHolder[() => FullR]
}

trait BuilderNodeAndHolder[R, RFn] extends BuilderNode[R, RFn] with BuilderNodeHolder[R, RFn]

object ReportableHelper {
  implicit def toReportableHelper[Params, BFn, R, RFn](r: NestedHolder[Reportable] with Reportable) = new ReportableHelper[Params, BFn, R, RFn](r)
  implicit def toReportableHelper[Params, BFn, R, RFn](r: BuilderNodeHolder[R, RFn] with Reportable) = new ReportableHelper[Params, BFn, R, RFn](r.asInstanceOf[NestedHolder[Reportable] with Reportable])
}

case class DocumentHolder(val nodes: List[Document], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable
case class EngineHolder(val nodes: List[Engine[_, _, _, _]], textOrder: Int = Reportable.nextTextOrder) extends NestedHolder[Reportable] with Reportable

class ReportableHelper[Params, BFn, R, RFn](r: NestedHolder[Reportable] with Reportable) {
  lazy val scenarios = r.all(classOf[Scenario[Params, BFn, R, RFn]]).toList.sortBy(_.textOrder)
  lazy val useCases = r.all(classOf[UseCase[R, RFn]]).toList.sortBy(_.textOrder)
  lazy val engines = r.all(classOf[Engine[Params, BFn, R, RFn]]).toList.sortBy(_.textOrder)
  lazy val documents = r.foldLeft(Set[Document]())((acc, r) =>
    r match { case r: Requirement => acc ++ r.references.flatMap(_.document); case _ => acc }).toList.sortBy((x) => x.titleString)

  trait DocumentAndEngineHolder {
    val documentHolder = DocumentHolder(documents)
    val engineHolder = EngineHolder(r.all(classOf[Engine[_, _, _, _]]).toList.sortBy(_.textOrder))
  }
  
  /** path(thisObject) then a document holder with all the documents under this object are called followed by an EngineHolder with all the engines under it (nested as needed) */
  def documentsAndEnginePaths = new DocumentAndEngineHolder with Traversable[List[Reportable]] {
    def foreach[U](fn: List[Reportable] => U): Unit = {
      val initialPath = List(r)
      val engineHolderPath = engineHolder :: initialPath
      fn(initialPath)
      fn(documentHolder :: initialPath)
      documents.map(_ :: initialPath).foreach(fn)
      fn(engineHolderPath)
      for (e <- engineHolder) e match {
        case f: FoldingEngine[_, _, _, _, _] =>
          fn(f :: engineHolderPath); for (e <- f.engines) fn(e :: f :: engineHolderPath)
        case _ => fn(e :: engineHolderPath)
      }
    }
  }
  import StartChildEndType._
  def documentsAndEngineStartChildEndPaths = new DocumentAndEngineHolder with Traversable[(List[Reportable], StartChildEndType)] {
    def foreach[U](fn: ((List[Reportable], StartChildEndType)) => U): Unit = {
      val initialPath = List(r)
      val engineHolderPath = engineHolder :: initialPath
      fn((initialPath, Start))
      fn((documentHolder :: initialPath, Start))
      documents.map((d) => (d :: initialPath, Child)).foreach(fn)
      fn((documentHolder :: initialPath, End))
      fn((engineHolderPath, Start))
      for (e <- engineHolder) e match {
        case f: FoldingEngine[_, _, _, _, _] =>
          val fPath = f :: engineHolderPath
          fn((fPath, Start)); for (e <- f.engines) fn((e :: fPath, Child)); fn((fPath, End))
        case _ => fn((e :: engineHolderPath, Child))
      }
      fn((engineHolderPath, End))
      fn((initialPath, End))
    }
  }
}

case class Project(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val nodes: List[Reportable] = List(),
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement with NestedHolder[Reportable] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Project(title, description, priority, nodes, references, textOrder)
  override def toString = s"Project(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case p: Project => Requirement.areRequirementFieldsEqual(this, p)
    case _ => false
  }
}

case class FoldingEngineDescription[Params, BFn, R, RFn, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: (FullR, R) => FullR,
  val initialValue: CodeHolder[() => FullR],
  val textOrder: Int = Reportable.nextTextOrder)
  extends EngineAsRequirement[Params, BFn, R, RFn] with FoldingBuilderNodeAndHolder[R, RFn, FullR] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new FoldingEngineDescription[Params, BFn, R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  override def toString = s"FoldingEngineDescription(${initialValue.description}, $foldingFn, nodes=${nodes.mkString(", ")}"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case fe: FoldingEngineDescription[Params, BFn, R, RFn, FullR] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, fe) &&
      (foldingFn == fe.foldingFn) &&
      (initialValue == fe.initialValue)
    case _ => false
  }
}
case class EngineDescription[Params, BFn, R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder)
  extends EngineAsRequirement[Params, BFn, R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn] =
    new EngineDescription[Params, BFn, R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case ed: EngineDescription[Params, BFn, R, RFn] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, ed)
    case _ => false
  }
  override def toString = s"EngineDescription(${title.getOrElse("")}, nodes=${nodes.mkString(",")})"
}

case class UseCase[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  override def toString = s"UseCase(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case uc: UseCase[R, RFn] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, uc)
    case _ => false
  }
}

case class Scenario[Params, BFn, R, RFn](
  val params: Params,
  val title: Option[String] = None,
  val description: Option[String] = None,
  val because: Option[CodeHolder[BFn]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = List(),
  val configurators: List[(Params) => Unit] = List(),
  val textOrder: Int = Reportable.nextTextOrder) extends BuilderNode[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)
  def copyScenario(because: Option[CodeHolder[BFn]] = because, assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = assertions, configurators: List[(Params) => Unit] = configurators) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators, textOrder)

  def actualCode(expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]) = code.getOrElse(expectedToCode(expected.getOrElse(throw NoExpectedException(this))))
  def executeConfigurators = configurators.foreach((c) => c(params))
  override def hashCode = (title.hashCode() + params.hashCode()) / 2
  override def equals(other: Any) = other match {
    case s: Scenario[Params, BFn, R, RFn] => Requirement.areBuilderNodeFieldsEquals(this, s) &&
      (s.params == params) && (s.because == because) && (s.assertions == assertions) && (s.configurators == configurators) && (s.expected == expected)
    case _ => false
  }
  override def toString = s"Scenario($params,$title,$description,$because,$code,$priority,$expected,$references,$assertions,$configurators)"
}

case class Document(
  val title: Option[String] = None,
  val description: Option[String] = None,
  val priority: Option[Int] = None,
  val url: Option[String] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Reportable.nextTextOrder) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Document(title, description, priority, url, references)
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case d: Document => Requirement.areRequirementFieldsEqual(this, d) && (url == d.url)
    case _ => false
  }

}

/** A reference is usually a link to a document. The 'ref' is a string of the form a.b.c..., where the fragments are string not containing a dot. The sort order is based on each fragment*/
case class Reference(ref: String = "", document: Option[Document] = None) extends Comparable[Reference] {
  def compareTo(other: Reference): Int = {
    val left = ref.split("\\.")
    val right = other.ref.split("\\.")
    val zipped = left.zipAll(right, "0", "0")
    zipped.map((f) => {
      val (l, r) = f
      try {
        val lInt = l.toInt
        val rInt = r.toInt
        lInt - rInt
      } catch {
        case e: Throwable => {
          l.compareTo(r)
        }
      }
    }).find((f) => f != 0).getOrElse(0)
  }

}


