package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
trait Reportable {
  val textOrder = Reportable.count.getAndIncrement()
}

object Reportable {
  val count = new AtomicInteger(0)
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
}

trait EngineNode[R, RFn] extends Requirement {
  def expected: Option[Either[Class[_ <: Exception], R]]
  def code: Option[CodeHolder[RFn]]
  def copyEngineNode(
    expected: Option[Either[Class[_ <: Exception], R]] = expected,
    code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn]
}
trait EngineNodeHolder[R, RFn] extends Traversable[EngineNode[R, RFn]] with Reportable {
  def nodes: List[EngineNode[R, RFn]]
  def copyNodes(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn]
  def foreach[U](f: EngineNode[R, RFn] => U): Unit = {
    for (c <- nodes) c match {
      case holder: EngineNodeHolder[R, RFn] =>
        f(c); holder.foreach(f)
      case c => f(c)
    }
  }

  def all[C <: EngineNode[R, RFn]](clazz: Class[C]) = this.collect { case c: C if (clazz.isAssignableFrom(c.getClass())) => c }

  val paths = new Traversable[List[Reportable]] {
    def foreach[U](f: List[Reportable] => U): Unit = foreachPrim(f, List[Reportable](EngineNodeHolder.this))

    def foreachPrim[U](f: List[Reportable] => U, path: List[Reportable]): Unit = path.head match {
      case holder: EngineNodeHolder[R, RFn] =>
        f(path)
        for (c <- holder.nodes)
          foreachPrim(f, c :: path)
      case _ => f(path)
    }
  }
}
trait EngineNodeAndHolder[R, RFn] extends EngineNode[R, RFn] with EngineNodeHolder[R, RFn]

case class EngineDescription[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[EngineNode[R, RFn]] = List(),
  val expected: Option[Either[Class[_ <: Exception], R]] = None,
  val references: Set[Reference] = Set())
  extends EngineNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyEngineNode(expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyNodes(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)

}

case class UseCase[R, RFn](
  title: Option[String] = None,
  description: Option[String] = None,
  code: Option[CodeHolder[RFn]] = None,
  priority: Option[Int] = None,
  nodes: List[EngineNode[R, RFn]] = List(),
  expected: Option[Either[Class[_ <: Exception], R]] = None,
  references: Set[Reference] = Set()) extends EngineNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyEngineNode(expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyNodes(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references)
  override def toString = s"UseCase(${title.getOrElse("None")}, nodes=(${nodes.mkString(",")}))"
}

case class Scenario[Params, BFn, R, RFn](
  val params: Params,
  val title: Option[String] = None,
  val description: Option[String] = None,
  val because: Option[CodeHolder[BFn]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val expected: Option[Either[Class[_ <: Exception], R]] = None,
  val references: Set[Reference] = Set()) extends EngineNode[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references)
  def copyEngineNode(expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references)
  def copyScenario(because: Option[CodeHolder[BFn]]) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references)

}

case class Document(
  title: Option[String] = None,
  description: Option[String] = None,
  priority: Option[Int] = None,
  url: Option[String] = None,
  references: Set[Reference] = Set()) extends Requirement {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Document(title, description, priority, url, references)

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
