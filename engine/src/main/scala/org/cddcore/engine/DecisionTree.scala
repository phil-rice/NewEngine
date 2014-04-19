package org.cddcore.engine

import scala.language.implicitConversions

class DecisionTreeLens[Params, BFn, R, RFn](val creator: (DecisionTreeNode[Params, BFn, R, RFn]) => DecisionTree[Params, BFn, R, RFn] = (root: DecisionTreeNode[Params, BFn, R, RFn]) => new SimpleDecisionTree(root, false)) {
  val rootL = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (tree) => tree.root,
    (tree, root) => creator(root),
    Some("rootL"))

  //  def exceptionMapL(asRequirement: BuilderNodeHolder[R, RFn]) =
  //    Lens[DecisionTree[Params, BFn, R, RFn],ExceptionMap](
  //      (tree) => tree.buildExceptions,
  //      (tree, buildExceptions) => creator(asRequirement)(tree.root, buildExceptions),
  //      Some("exceptionMapL"))

  def toDecisionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Decision[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Decision[Params, BFn, R, RFn]],
    (d, x) => x,
    Some("toDecisionL"))
  def toConclusionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Conclusion[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Conclusion[Params, BFn, R, RFn]],
    (d, x) => x,
    Some("toConclusionL"))
  def yesL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.yes,
    (d, x) => d.copy(yes = x),
    Some("yesL"))
  def noL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.no,
    (d, x) => d.copy(no = x),
    Some("noL"))
}

trait DecisionTree[Params, BFn, R, RFn] {
  def root: DecisionTreeNode[Params, BFn, R, RFn]
  def rootIsDefault: Boolean

}

case class SimpleDecisionTree[Params, BFn, R, RFn](root: DecisionTreeNode[Params, BFn, R, RFn], val rootIsDefault: Boolean = true) extends DecisionTree[Params, BFn, R, RFn]

sealed trait DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios: List[Scenario[Params, BFn, R, RFn]]
}

case class Conclusion[Params, BFn, R, RFn](code: CodeHolder[RFn], scenarios: List[Scenario[Params, BFn, R, RFn]]) extends DecisionTreeNode[Params, BFn, R, RFn]

case class Decision[Params, BFn, R, RFn](because: List[CodeHolder[BFn]], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn], scenarioThatCausedNode: Scenario[Params, BFn, R, RFn]) extends DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios = yes.scenarios ++ no.scenarios
  def isTrue(bc: (BFn) => Boolean) = because.foldLeft(false)((acc, ch) => acc || bc(ch.fn))
}

trait MakeClosures[Params, BFn, R, RFn] {
  type S = Scenario[Params, BFn, R, RFn]
  type Result = Either[Exception, R]
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R

  def safe[T](block: => T): Either[Exception, T] = try Right(block) catch { case e: Exception => Left(e) }

  def makeBecauseClosure(params: Params): BecauseClosure
  def makeBecauseClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): BecauseClosure

  protected def wrapBecause[X](s: S, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseScenarioException(s, e) }
  protected def wrapBecause[X](p: Params, x: => X): X = try { x } catch { case e: Exception => throw BecauseClauseException(p, e) }

  def makeResultClosure(params: Params): ResultClosure
  def makeResultClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): ResultClosure

  def evaluateBecause(scenarioWithbecause: Scenario[Params, BFn, R, RFn], scenarioWithParams: Scenario[Params, BFn, R, RFn]): Boolean =
    makeBecauseClosure(scenarioWithParams)(scenarioWithbecause.
      because.getOrElse(throw new IllegalArgumentException(s"Scenario doesn't have because\scenarioWithbecause")).fn)
  def evaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = makeResultClosure(scenarioWithParams)(rfn)
  def safeEvaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = safe(makeResultClosure(scenarioWithParams)(rfn))
}

case class SimpleEvaluateTree[Params, BFn, R, RFn](
  val makeClosures: MakeClosures[Params, BFn, R, RFn],
  val lens: DecisionTreeLens[Params, BFn, R, RFn],
  val validator: ValidateScenario[Params, BFn, R, RFn]) extends EvaluateTree[Params, BFn, R, RFn] {
}

trait EvaluateTree[Params, BFn, R, RFn] {
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]

  val makeClosures: MakeClosures[Params, BFn, R, RFn]
  val validator: ValidateScenario[Params, BFn, R, RFn]
  val lens: DecisionTreeLens[Params, BFn, R, RFn]
  import makeClosures._
  import lens._

  def findLensToConclusion(root: DTN, s: S): Lens[DT, DTN] = findLensToConclusion(root, makeBecauseClosure(s), rootL)
  def findLensToConclusion(root: DTN, bc: (BFn) => Boolean): Lens[DT, DTN] = findLensToConclusion(root, bc, rootL)
  def findLensToLastDecisionNode(root: DTN, s: S): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] = findLensToLastDecisionNode(root, makeBecauseClosure(s), rootL.andThen(toDecisionL))

  def safeEvaluate(tree: DT, scenarioWithParams: S) = safe(evaluate(tree, scenarioWithParams))

  def evaluate(tree: DT, scenarioWithParams: S): R = {
    val bc = makeBecauseClosure(scenarioWithParams)
    val c = findConclusion(tree, bc).code
    makeResultClosure(scenarioWithParams).apply(c.fn)
  }
  def evaluate(tree: DT, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(tree, bc).code
    makeResultClosure(params).apply(c.fn)
  }

  def findConclusion(tree: DT, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = findConclusion(tree.root, bc)

  def findConclusion(root: DTN, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = {
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => c
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findConclusion(d.yes, bc)
        case false => findConclusion(d.no, bc)
      }
    }
  }

  type LensToNode = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]]

  protected def findLensToConclusion(root: DTN, bc: BecauseClosure, soFar: LensToNode): Lens[DT, DTN] =
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => soFar
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findLensToConclusion(d.yes, bc, soFar.andThen(toDecisionL).andThen(yesL))
        case false => findLensToConclusion(d.no, bc, soFar.andThen(toDecisionL).andThen(noL))
      }
    }
  protected def findLensToLastDecisionNode(root: DTN, bc: BecauseClosure, soFar: Lens[DT, Decision[Params, BFn, R, RFn]]): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] =
    root match {
      case d: Decision[Params, BFn, R, RFn] => {
        def returnMeOrRecurse(child: DTN, nextSoFar: LensToNode): Option[Lens[DT, Decision[Params, BFn, R, RFn]]] =
          child match {
            case c: Conclusion[Params, BFn, R, RFn] => Some(soFar)
            case d: Decision[Params, BFn, R, RFn] => findLensToLastDecisionNode(child, bc, nextSoFar.andThen(toDecisionL))
          }
        d.isTrue(bc) match {
          case true => returnMeOrRecurse(d.yes, soFar.andThen(yesL))
          case false => returnMeOrRecurse(d.no, soFar.andThen(noL))
        }
      }
      case c: Conclusion[Params, BFn, R, RFn] => None
    }
}

