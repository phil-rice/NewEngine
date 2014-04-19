package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import org.cddcore.utilities.Maps
trait Reportable {
  val textOrder: Int = Reportable.count.getAndIncrement()
}

object Reportable {
  private val count = new AtomicInteger(0)
  def compare[R](left: Either[Exception, R], right: Either[Exception, R]) = {
    (left, right) match {
      case (Left(le), Left(re)) => le.getClass() == re.getClass()
      case (Right(lr), Right(rr)) => lr == rr
      case _ => false
    }
  }
}
object Requirement {
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

  def all[C <: Requirement](clazz: Class[C]) = this.collect { case c: C if (clazz.isAssignableFrom(c.getClass())) => c }

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
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set()) extends BuilderNodeAndHolder[R, RFn] {
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
  def copyScenario(because: Option[CodeHolder[BFn]] = because,
    assertions: List[CodeHolder[(Params, Either[Exception, R]) => Boolean]] = assertions,
    configurators: List[(Params) => Unit] = configurators) =
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

object ExceptionMap {
  def apply() = new ExceptionMap()
}
class ExceptionMap(val map: Map[Int, List[Exception]] = Map()) extends Function[Requirement, List[Exception]] {
  def apply(r: Requirement) = map(r.textOrder)
  def +(kv: (Requirement, Exception)) = kv match { case (r, e) => new ExceptionMap(Maps.addToList(map, r.textOrder, e)) }
  def ++(em: ExceptionMap) = {
    em.map.foldLeft(map)((acc, kv) => kv._2.foldLeft(acc)((acc, v) => Maps.addToList(acc, kv._1, v)))
  }
  def contains(r: Requirement) = map.contains(r.textOrder)
  def size = map.size
  override def hashCode() = map.hashCode
  override def equals(other: Any) = other match { case e: ExceptionMap => e.map == map; case _ => false }
  def toMap[Params, BFn, R, RFn](e: Engine[Params, BFn, R, RFn]) = {
    val textOrderToRequirement = e.asRequirement.all(classOf[Requirement]).foldLeft(Map[Int, Requirement]())((acc, r) => acc + (r.textOrder -> r))
    map.map((kv) => kv match {
      case (to, le) => textOrderToRequirement(to) -> le
    }).foldLeft(Map[Requirement, List[Exception]]()) { _ + _ }
  }
} 
