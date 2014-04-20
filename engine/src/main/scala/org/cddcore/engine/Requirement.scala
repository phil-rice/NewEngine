package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import org.cddcore.utilities.Maps
import org.cddcore.utilities.CodeHolder
import org.cddcore.utilities.NestedHolder
trait Reportable {
}

object Reportable {
  def compare[R](left: Either[Exception, R], right: Either[Exception, R]) = {
    (left, right) match {
      case (Left(le), Left(re)) => le.getClass() == re.getClass()
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
  }
}
object Requirement {
  final val count = new AtomicInteger(0)
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
  val textOrder: Int
  def title: Option[String]
  def description: Option[String]
  def priority: Option[Int]
  def references: Set[Reference]
  def copyRequirement(title: Option[String] = title,
    description: Option[String] = description,
    priority: Option[Int] = priority,
    references: Set[Reference] = references): Requirement
  lazy val titleString = title.getOrElse("")
}

trait BuilderNode[R, RFn] extends Requirement {
  def expected: Option[Either[Exception, R]]
  def code: Option[CodeHolder[RFn]]
  def copyBuilderNode(
    expected: Option[Either[Exception, R]] = expected,
    code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn]
}

trait BuilderNodeHolder[R, RFn] extends NestedHolder[BuilderNode[R, RFn], BuilderNodeHolder[R, RFn]] 

trait FoldingBuilderNodeAndHolder[R, RFn, FullR] extends BuilderNodeAndHolder[R, RFn] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: CodeHolder[() => FullR]
}

trait BuilderNodeAndHolder[R, RFn] extends BuilderNode[R, RFn] with BuilderNodeHolder[R, RFn]

case class FoldingEngineDescription[R, RFn, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: (FullR, R) => FullR,
  val initialValue: CodeHolder[() => FullR],
  val textOrder: Int = Requirement.count.getAndIncrement)
  extends BuilderNodeAndHolder[R, RFn] with FoldingBuilderNodeAndHolder[R, RFn, FullR] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  override def toString = s"FoldingEngineDescription(${initialValue.description}, $foldingFn, nodes=${nodes.mkString(", ")}"
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case fe: FoldingEngineDescription[R, RFn, FullR] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, fe) &&
      (foldingFn == fe.foldingFn) &&
      (initialValue == fe.initialValue)
    case _ => false
  }

}
case class EngineDescription[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val textOrder: Int = Requirement.count.getAndIncrement())
  extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references, textOrder)
  override def hashCode = (title.hashCode() + description.hashCode()) / 2
  override def equals(other: Any) = other match {
    case ed: EngineDescription[R, RFn] => Requirement.areBuilderNodeAndHolderFieldsEqual(this, ed)
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
  val textOrder: Int = Requirement.count.getAndIncrement()) extends BuilderNodeAndHolder[R, RFn] {
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
  val textOrder: Int = Requirement.count.getAndIncrement()) extends BuilderNode[R, RFn] {
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
  val textOrder: Int = Requirement.count.getAndIncrement()) extends Requirement {
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


