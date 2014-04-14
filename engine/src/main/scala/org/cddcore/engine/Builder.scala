package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.util.Sorting

trait Builder[R, RFn, B <: Builder[R, RFn, B]] extends EngineNodeHolder[R, RFn] {
  implicit def ldp: LoggerDisplayProcessor
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
  def expectException(e: Exception, title: String = null): B = currentNodeL.andThen(expectedL).set(this, Some(Left(e))).asInstanceOf[B]
  def copyNodes(nodes: List[EngineNode[R, RFn]]): B
  def codeHolder(codeHolder: CodeHolder[RFn]): B = currentNodeL.andThen(codeL).set(this, Some(codeHolder)).asInstanceOf[B]
  def reference(ref: String): B =
    currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, None)).asInstanceOf[B]
  def reference(ref: String, document: Document): B =
    currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, Some(document))).asInstanceOf[B]
}

case class ModifedChildrenEngineNodeHolder[R, RFn](nodes: List[EngineNode[R, RFn]] = List()) extends EngineNodeHolder[R, RFn] {
  def copyNodes(nodes: List[EngineNode[R, RFn]]) = throw new IllegalStateException
}

trait ValidateScenario[Params, BFn, R, RFn] extends MakeClosures[Params, BFn, R, RFn] {
  private type S = Scenario[Params, BFn, R, RFn]
  def scenarios: Set[S]
  def validateScenario(s: S)(implicit ldp: LoggerDisplayProcessor) = {
    if (!s.expected.isDefined)
      throw NoExpectedException(s)
    checkBecause(s)
    checkHasExpected(s)
  }
  def checkDuplicateScenario(s: S) = {
    if (scenarios.contains(s)) throw DuplicateScenarioException(s)
    s
  }
  def checkBecause(s: S)(implicit ldp: LoggerDisplayProcessor) = {
    s.because match {
      case Some(CodeHolder(bfn, _, _)) => if (!evaluteBecause(bfn, s.params)) throw ScenarioBecauseException(s);
      case _ =>
    }
    s
  }
  def checkHasExpected(s: S) = {
    if (s.expected.isEmpty) throw NoExpectedException(s)
    s

  }

}

trait BuilderWithModifyChildrenForBuild[R, RFn] extends EngineNodeHolder[R, RFn] {
  def modifyChildrenForBuild: ModifedChildrenEngineNodeHolder[R, RFn] = {
    def modifyChildAsNode(path: List[Reportable], child: EngineNode[R, RFn]) = {
      child
    }
    def firstOption[X](path: List[Reportable], fn: (EngineNode[R, RFn]) => Option[X]): Option[X] = {
      path.collect { case e: EngineNode[R, RFn] => fn(e) }.find((r) => r.isDefined).getOrElse(None)
    }
    def modifyChild(path: List[Reportable]): EngineNode[R, RFn] = {
      val nodeModified = path.head match {
        case node: EngineNode[R, RFn] => node.copyEngineNode(
          expected = firstOption(path, _.expected),
          code = firstOption(path, _.code)).
          copyRequirement(
            priority = firstOption(path, _.priority))
        case x => x
      }
      val withChildren = nodeModified match {
        case holder: EngineNodeHolder[R, RFn] => holder.copyNodes(nodes = modifyChildren(path, holder))
        case x: EngineNode[R, RFn] => x
      }
      withChildren.asInstanceOf[EngineNode[R, RFn]]
    }
    def modifyChildren(path: List[Reportable], holder: EngineNodeHolder[R, RFn]): List[EngineNode[R, RFn]] =
      holder.nodes.map((x) => modifyChild(x :: path)).sorted(Ordering.by((x: EngineNode[R, RFn]) => (-x.priority.getOrElse(0), -x.textOrder)))
    new ModifedChildrenEngineNodeHolder(modifyChildren(List(), this))
  }
}