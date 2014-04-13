package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

trait Builder[R, RFn, B <: Builder[R, RFn, B]] extends EngineNodeHolder[R, RFn] {
  val bl = new BuilderLens[R, RFn, Builder[R, RFn, B]]
  import bl._

  def title(title: String): B = {
    val result = currentNodeL.andThen(asRequirementL).andThen(titleL).set(this, Some(title)).asInstanceOf[B];
    result
  }
  def description(description: String): B = currentNodeL.andThen(asRequirementL).andThen(descriptionL).set(this, Some(description)).asInstanceOf[B]
  def priority(priority: Int): B = currentNodeL.andThen(asRequirementL).andThen(priorityL).set(this, Some(priority)).asInstanceOf[B]

  def useCase(title: String): B = nextUseCaseHolderL.andThen(nodesL).mod(this, (nodes: List[EngineNode[R, RFn]]) => new UseCase[R, RFn](Some(title)) :: nodes).asInstanceOf[B]
  def expected(r: R, title: String = null): B = currentNodeL.andThen(expectedL).set(this, Some(Right(r))).asInstanceOf[B]
  def expectException(e: Class[_ <: Exception], title: String = null): B = currentNodeL.andThen(expectedL).set(this, Some(Left(e))).asInstanceOf[B]
  def copyNodes(nodes: List[EngineNode[R, RFn]]): B
  def codeHandler(codeHolder: CodeHolder[RFn]): B = currentNodeL.andThen(codeL).set(this, Some(codeHolder)).asInstanceOf[B]
  def reference(ref: String): B =
    currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, None)).asInstanceOf[B]
  def reference(ref: String, document: Document): B =
    currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, Some(document))).asInstanceOf[B]
}

