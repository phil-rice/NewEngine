package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.util.Sorting
import org.cddcore.utilities.Maps
trait HasExceptionMap[R, RFn] {
  def buildExceptions: ExceptionMap
}
trait CanCopyWithNewExceptionMap[R, RFn] extends HasExceptionMap[R, RFn] {
  def copyWithNewExceptions(buildExceptions: ExceptionMap): CanCopyWithNewExceptionMap[R, RFn]
}
trait Builder[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]]
  extends BuilderNodeHolder[R, RFn]
  with CanCopyWithNewExceptionMap[R, RFn]
  with WhileBuildingValidateScenario[Params, BFn, R, RFn] {
  implicit def ldp: LoggerDisplayProcessor
  val bl = new FullBuilderLens[Params, BFn, R, RFn, FullR, Builder[Params, BFn, R, RFn, FullR, B, E]]
  import bl._
  lazy val scenarios = all(classOf[Scenario[Params, BFn, R, RFn]]).toList

  protected def wrap(stuff: => Builder[Params, BFn, R, RFn, FullR, B, E]): B = try {
    stuff.asInstanceOf[B]
  } catch {
    case e: Exception => {
      Engine.testing match {
        case true => {
          val current = currentNodeL.get(this)
          val result = builderToCanCopyWithNewExceptionMapL.andThen(exceptionMap).mod(this, (map) =>
            map + (current -> e)).asInstanceOf[B]
          result
        }
        case false => throw e
      }
    }
  }

  def title(title: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(titleL).set(this, Some(title)))
  def description(description: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(descriptionL).set(this, Some(description)))
  def priority(priority: Int): B = wrap(currentNodeL.andThen(asRequirementL).andThen(priorityL).set(this, Some(priority)))

  def useCase(title: String, description: String = null): B = wrap(nextUseCaseHolderL.andThen(nodesL).mod(this, (nodes: List[BuilderNode[R, RFn]]) =>
    new UseCase[R, RFn](Some(title), description = Option(description)) :: nodes))
  def expected(r: R, title: String = null): B = wrap(currentNodeL.andThen(expectedL).set(this, Some(Right(r))))
  def expectException(e: Exception, title: String = null): B = wrap(currentNodeL.andThen(expectedL).set(this, Some(Left(e))))

  def reference(ref: String): B = wrap(currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, None)))
  def reference(ref: String, document: Document): B = wrap(currentNodeL.andThen(asRequirementL).andThen(referencesL).mod(this, (r) => r + Reference(ref, Some(document))))

  def copyNodes(nodes: List[BuilderNode[R, RFn]]): B
  def codeHolder(codeHolder: CodeHolder[RFn]): B = wrap(currentNodeL.andThen(codeL((o, n, c) => {})).set(this, Some(codeHolder)))
  def childEngine(title: String): B = wrap(toFoldingEngineDescription.andThen(foldEngineNodesL).
    mod(this.asInstanceOf[B], ((n) => new EngineDescription[R, RFn](title = Some(title)) :: n)).asInstanceOf[Builder[Params, BFn, R, RFn, FullR, B, E]])
}

trait WhileBuildingValidateScenario[Params, BFn, R, RFn] {
  type S = Scenario[Params, BFn, R, RFn]
  type MC = MakeClosures[Params, BFn, R, RFn]
  def checkDuplicateScenario(scenarios: List[S], s: S) = {
    if (scenarios.contains(s)) throw DuplicateScenarioException(s)
    s
  }
  def checkBecause(mc: MC, s: S)(implicit ldp: LoggerDisplayProcessor) = {
    s.because match {
      case Some(_) => if (!mc.evaluateBecause(s, s)) throw ScenarioBecauseException(s);
      case _ =>
    }
    s
  }
}

class SimpleValidateScenario[Params, BFn, R, RFn] extends ValidateScenario[Params, BFn, R, RFn]

trait ValidateScenario[Params, BFn, R, RFn] extends WhileBuildingValidateScenario[Params, BFn, R, RFn] {
  def preValidateScenario(mc: MC, s: S)(implicit ldp: LoggerDisplayProcessor) = {
    if (!s.expected.isDefined)
      throw NoExpectedException(s)
    checkBecause(mc, s)
    checkHasExpected(s)
  }
  def postValidateScenario(evaluateTree: EvaluateTree[Params, BFn, R, RFn], tree: DecisionTree[Params, BFn, R, RFn], s: S)(implicit ldp: LoggerDisplayProcessor) = {
    checkAssertions(evaluateTree, tree, s)
    checkCorrectValue(evaluateTree, tree, s)
  }

  def checkHasExpected(s: S) = {
    if (s.expected.isEmpty) throw NoExpectedException(s)
    s
  }
  def checkAssertions(evaluateTree: EvaluateTree[Params, BFn, R, RFn], tree: DecisionTree[Params, BFn, R, RFn], s: S) = {
    s.assertions.foreach((a) => {
      val result = evaluateTree.safeEvaluate(tree, s)
      val assertionResult = a.fn(s.params, result)
      if (!assertionResult) throw AssertionException(a, s)
    })
  }
  def checkCorrectValue(evaluateTree: EvaluateTree[Params, BFn, R, RFn], tree: DecisionTree[Params, BFn, R, RFn], s: S) = {
    val actual = evaluateTree.safeEvaluate(tree, s)
    s.expected match {
      case Some(ex) => if (!Reportable.compare(ex, actual))
        actual match {
          case Left(cause) => throw CameToWrongConclusionScenarioException(ex, actual, s, cause)
          case _ => throw CameToWrongConclusionScenarioException(ex, actual, s, null)
        }
      case _ => throw NoExpectedException(s)
    }
  }
}

class SimpleBuilderWithModifyChildrenForBuild[R, RFn] extends BuilderWithModifyChildrenForBuild[R, RFn]

trait BuilderWithModifyChildrenForBuild[R, RFn] {
  def modifyChildrenForBuild(requirement: BuilderNodeAndHolder[R, RFn]): BuilderNodeAndHolder[R, RFn] = {
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
      holder.nodes.map((x) => modifyChild(x :: path)).sortBy(-_.textOrder)
    modifyChild(List(requirement)).asInstanceOf[BuilderNodeAndHolder[R, RFn]]
  }
}