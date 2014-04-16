package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.util.Sorting
import org.cddcore.utilities.Maps
trait HasExceptionMap[R, RFn] {
  def buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]
}
trait CanCopyWithNewExceptionMap[R, RFn] extends HasExceptionMap[R, RFn] {
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]): CanCopyWithNewExceptionMap[R, RFn]
}
trait Builder[R, RFn, FullR, B <: Builder[R, RFn, FullR, B]] extends BuilderNodeHolder[R, RFn] with CanCopyWithNewExceptionMap[R, RFn] {
  implicit def ldp: LoggerDisplayProcessor
  val bl = new BuilderLens[R, RFn, FullR, Builder[R, RFn, FullR, B]]
  import bl._

  protected def wrap(stuff: => Builder[R, RFn, FullR, B]): B = try {
    stuff.asInstanceOf[B]
  } catch {
    case e: Exception => {
      Engine.testing match {
        case true => {
          val current = currentNodeL.get(this)
          val result = builderToCanCopyWithNewExceptionMapL.andThen(exceptionMap).mod(this, (map) =>
            Maps.addToList(map, current, e)).asInstanceOf[B]
          result
        }
        case false => throw e
      }
    }
  }

  def title(title: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(titleL).set(this, Some(title)))
  def description(description: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(descriptionL).set(this, Some(description)))
  def priority(priority: Int): B = wrap(currentNodeL.andThen(asRequirementL).andThen(priorityL).set(this, Some(priority)))

  def useCase(title: String): B = wrap(nextUseCaseHolderL.andThen(nodesL).mod(this, (nodes: List[BuilderNode[R, RFn]]) => new UseCase[R, RFn](Some(title)) :: nodes))
  def expected(r: R, title: String = null): B = wrap(currentNodeL.andThen(expectedL).set(this, Some(Right(r))))
  def expectException(e: Exception, title: String = null): B = wrap(currentNodeL.andThen(expectedL).set(this, Some(Left(e))))

  def reference(ref: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, None)))
  def reference(ref: String, document: Document): B = wrap(currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, Some(document))))

  def copyNodes(nodes: List[BuilderNode[R, RFn]]): B
  def codeHolder(codeHolder: CodeHolder[RFn]): B = wrap(currentNodeL.andThen(codeL((o, n, c) => {})).set(this, Some(codeHolder)))
  def childEngine(title: String): B =
    wrap(
      toFoldingEngineDescription.andThen(foldEngineNodesL).mod(this.asInstanceOf[B], ((n) => new EngineDescription[R, RFn](title = Some(title)) :: n)).asInstanceOf[Builder[R, RFn, FullR, B]])

}

case class ModifedChildrenBuilderNodeHolder[R, RFn](nodes: List[BuilderNode[R, RFn]] = List()) extends BuilderNodeHolder[R, RFn] {
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) = throw new IllegalStateException
}

trait ValidateScenario[Params, BFn, R, RFn] extends MakeClosures[Params, BFn, R, RFn] {
  private type S = Scenario[Params, BFn, R, RFn]
  def scenarios: Set[S]
  def preValidateScenario(s: S)(implicit ldp: LoggerDisplayProcessor) = {
    if (!s.expected.isDefined)
      throw NoExpectedException(s)
    checkBecause(s)
    checkHasExpected(s)
  }
  def postValidateScenario(tree: DecisionTree[Params, BFn, R, RFn], s: S)(implicit ldp: LoggerDisplayProcessor) = {
    checkAssertions(tree, s)
    checkCorrectValue(tree, s)
  }
  def checkDuplicateScenario(s: S) = {
    if (scenarios.contains(s)) throw DuplicateScenarioException(s)
    s
  }
  def checkBecause(s: S)(implicit ldp: LoggerDisplayProcessor) = {
    s.because match {
      case Some(_) => if (!evaluateBecause(s, s)) throw ScenarioBecauseException(s);
      case _ =>
    }
    s
  }
  def checkHasExpected(s: S) = {
    if (s.expected.isEmpty) throw NoExpectedException(s)
    s
  }
  def checkAssertions(tree: DecisionTree[Params, BFn, R, RFn], s: S) = {
    s.assertions.foreach((a) => {
      val result = tree.safeEvaluate(s)
      val assertionResult = a.fn(s.params, result)
      if (!assertionResult) throw AssertionException(a, s)
    })
  }
  def checkCorrectValue(tree: DecisionTree[Params, BFn, R, RFn], s: S) = {
    val actual = tree.safeEvaluate(s)
    s.expected match {
      case Some(ex) => if (!Reportable.compare(ex, actual)) throw CameToWrongConclusionScenarioException(ex, actual, s)
      case _ => throw NoExpectedException(s)
    }
  }

}

trait BuilderWithModifyChildrenForBuild[R, RFn] extends BuilderNodeHolder[R, RFn] {
  def modifyChildrenForBuild: ModifedChildrenBuilderNodeHolder[R, RFn] = {
    def modifyChildAsNode(path: List[Reportable], child: BuilderNode[R, RFn]) = {
      child
    }
    def firstOption[X](path: List[Reportable], fn: (BuilderNode[R, RFn]) => Option[X]): Option[X] = {
      path.collect { case e: BuilderNode[R, RFn] => fn(e) }.find((r) => r.isDefined).getOrElse(None)
    }
    def modifyChild(path: List[Reportable]): BuilderNode[R, RFn] = {
      val nodeModified = path.head match {
        case node: BuilderNode[R, RFn] => node.copyBuilderNode(
          expected = firstOption(path, _.expected),
          code = firstOption(path, _.code)).
          copyRequirement(
            priority = firstOption(path, _.priority))
        case x => x
      }
      val withChildren = nodeModified match {
        case holder: BuilderNodeHolder[R, RFn] => holder.copyNodes(nodes = modifyChildren(path, holder))
        case x: BuilderNode[R, RFn] => x
      }
      withChildren.asInstanceOf[BuilderNode[R, RFn]]
    }
    def modifyChildren(path: List[Reportable], holder: BuilderNodeHolder[R, RFn]): List[BuilderNode[R, RFn]] =
      holder.nodes.map((x) => modifyChild(x :: path)).sorted(Ordering.by((x: BuilderNode[R, RFn]) => (-x.priority.getOrElse(0), -x.textOrder)))
    new ModifedChildrenBuilderNodeHolder(modifyChildren(List(), this))
  }
}