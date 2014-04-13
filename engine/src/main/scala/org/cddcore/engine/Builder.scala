package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

trait EngineNode[R, RFn] {
  def title: Option[String]
  def description: Option[String]
  def priority: Option[Int]
  def expected: Option[Either[Class[_ <: Exception], R]]
  def code: Option[CodeHolder[RFn]]
  def copy(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn]
}
trait EngineNodeHolder[R, RFn] {
  def nodes: List[EngineNode[R, RFn]]
  def copy(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn]
}
trait EngineNodeAndHolder[R, RFn] extends EngineNode[R, RFn] with EngineNodeHolder[R, RFn]

case class EngineDescription[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[EngineNode[R, RFn]] = List(),
  val expected: Option[Either[Class[_ <: Exception], R]] = None) extends EngineNodeAndHolder[R, RFn] {
  def copy(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected)
  def copy(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn] =
    new EngineDescription[R, RFn](title, description, code, priority, nodes, expected)

}

case class UseCase[R, RFn](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[EngineNode[R, RFn]] = List(),
  val expected: Option[Either[Class[_ <: Exception], R]] = None) extends EngineNodeAndHolder[R, RFn] {
  def copy(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected)
  def copy(nodes: List[EngineNode[R, RFn]]): EngineNodeHolder[R, RFn] =
    new UseCase[R, RFn](title, description, code, priority, nodes, expected)
}

case class Scenario[Params, BFn, R, RFn](
  val params: Params,
  val title: Option[String] = None,
  val description: Option[String] = None,
  val because: Option[CodeHolder[BFn]] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val expected: Option[Either[Class[_ <: Exception], R]] = None) extends EngineNode[R, RFn] {
  def copy(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, expected: Option[Either[Class[_ <: Exception], R]] = expected, code: Option[CodeHolder[RFn]] = code): EngineNode[R, RFn] =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected)

}

trait Builder[R, RFn, B <: Builder[R, RFn, B]] extends EngineNodeHolder[R, RFn] {
  val bl = new BuilderLens[R, RFn, Builder[R, RFn, B]]
  import bl._

  def title(title: String): B = {
    val result = currentNodeL.andThen(titleL).set(this, Some(title)).asInstanceOf[B];
    result
  }
  def description(description: String): B = currentNodeL.andThen(descriptionL).set(this, Some(description)).asInstanceOf[B]
  def priority(priority: Int): B = currentNodeL.andThen(priorityL).set(this, Some(priority)).asInstanceOf[B]

  def useCase(title: String): B =
    nextUseCaseHolderL.andThen(nodesL).mod(this, (nodes: List[EngineNode[R, RFn]]) =>
      new UseCase[R, RFn](Some(title)) :: nodes).asInstanceOf[B]
  def expected(r: R, title: String = null): B = currentNodeL.andThen(expectedL).set(this, Some(Right(r))).asInstanceOf[B]
  def expectException(e: Class[_ <: Exception], title: String = null): B = currentNodeL.andThen(expectedL).set(this, Some(Left(e))).asInstanceOf[B]
  def copy(nodes: List[EngineNode[R, RFn]]): B
  def codeHandler(codeHolder: CodeHolder[RFn]): B = currentNodeL.andThen(codeL).set(this, Some(codeHolder)).asInstanceOf[B]
}

