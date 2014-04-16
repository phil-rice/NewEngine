package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
trait Reportable {
  val textOrder = Reportable.count.getAndIncrement()
}

object Reportable {
  val count = new AtomicInteger(0)
  def compare[R](left: Either[Exception, R], right: Either[Exception, R]) = {
    (left, right) match {
      case (Left(le), Left(re)) => le.getClass() == re.getClass()
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
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
}

trait BuilderNode[R, RFn] extends Requirement {
  def expected: Option[Either[Exception, R]]
  def code: Option[CodeHolder[RFn]]
  def copyBuilderNode(
    expected: Option[Either[Exception, R]] = expected,
    code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn]
}

trait BuilderNodeHolder[R, RFn] extends Traversable[BuilderNode[R, RFn]] with Reportable {
  def nodes: List[BuilderNode[R, RFn]]
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn]
  def foreach[U](f: BuilderNode[R, RFn] => U): Unit = {
    for (c <- nodes) c match {
      case holder: BuilderNodeHolder[R, RFn] =>
        f(c); holder.foreach(f)
      case c => f(c)
    }
  }

  def all[C <: BuilderNode[R, RFn]](clazz: Class[C]) = this.collect { case c: C if (clazz.isAssignableFrom(c.getClass())) => c }

  val paths = new Traversable[List[Reportable]] {
    def foreach[U](f: List[Reportable] => U): Unit = foreachPrim(f, List[Reportable](BuilderNodeHolder.this))

    def foreachPrim[U](f: List[Reportable] => U, path: List[Reportable]): Unit = path.head match {
      case holder: BuilderNodeHolder[R, RFn] =>
        f(path)
        for (c <- holder.nodes)
          foreachPrim(f, c :: path)
      case _ => f(path)
    }
  }
}

trait BuilderNodeAndHolder[R, RFn] extends BuilderNode[R, RFn] with BuilderNodeHolder[R, RFn]

case class EngineDescription[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set())
  extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected, references)
  override def toString = s"EngineDescription(${title.getOrElse("")}, nodes=${nodes.mkString(",")})"

}

case class UseCase[R, RFn](
  title: Option[String] = None,
  description: Option[String] = None,
  code: Option[CodeHolder[RFn]] = None,
  priority: Option[Int] = None,
  nodes: List[BuilderNode[R, RFn]] = List(),
  expected: Option[Either[Exception, R]] = None,
  references: Set[Reference] = Set()) extends BuilderNodeAndHolder[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected, references)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]): BuilderNodeHolder[R, RFn] =
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
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = List(),
  val configurators: List[(Params) => Unit] = List()) extends BuilderNode[R, RFn] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references, assertions, configurators)

  def actualCode(expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]) = code.getOrElse(expectedToCode(expected.getOrElse(throw NoExpectedException(this))))
  def executeConfigurators = configurators.foreach((c) => c(params))
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
