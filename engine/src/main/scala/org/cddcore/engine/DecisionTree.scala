package org.cddcore.engine

import scala.language.implicitConversions

trait DecisionTreeLens[Params, BFn, R, RFn] {
  def creator(requirements: BuilderNodeHolder[R, RFn]): (DecisionTreeNode[Params, BFn, R, RFn], Map[BuilderNode[R, RFn], List[Exception]]) => DecisionTreeAndExceptions[Params, BFn, R, RFn];
  def rootL(requirements: BuilderNodeHolder[R, RFn], buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]) = Lens[DecisionTreeAndExceptions[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (tree) => tree.root,
    (tree, root) => creator(requirements)(root, buildExceptions),
    Some("rootL"))

  def exceptionMapL(requirements: BuilderNodeHolder[R, RFn]) =
    Lens[DecisionTreeAndExceptions[Params, BFn, R, RFn], Map[BuilderNode[R, RFn], List[Exception]]](
      (tree) => tree.buildExceptions,
      (tree, buildExceptions) => creator(requirements)(tree.root, buildExceptions),
      Some("exceptionMapL"))

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

trait DecisionTree[Params, BFn, R, RFn] extends EvaluateTree[Params, BFn, R, RFn] {
  def root: DTN
  def rootIsDefault: Boolean
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]

}



trait DecisionTreeAndExceptions[Params, BFn, R, RFn] extends DecisionTree[Params, BFn, R, RFn] with HasExceptionMap[ R, RFn] {
  val lens: DecisionTreeLens[Params, BFn, R, RFn]
  import lens._
 
  type LensToNode = Lens[DecisionTreeAndExceptions[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]]

  def findLensToConclusion(requirements: BuilderNodeHolder[R, RFn], buildExceptions: Map[BuilderNode[R, RFn], List[Exception]], bc: BecauseClosure): Lens[DT, DTN] =
    findLensToConclusion(root, bc, rootL(requirements, buildExceptions))

  protected def findLensToConclusion(root: DTN, bc: BecauseClosure, soFar: LensToNode): Lens[DT, DTN] =
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => soFar
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findLensToConclusion(d.yes, bc, soFar.andThen(toDecisionL).andThen(yesL))
        case false => findLensToConclusion(d.no, bc, soFar.andThen(toDecisionL).andThen(noL))
      }
    }
}

sealed trait DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios: List[Scenario[Params, BFn, R, RFn]]
}

case class Conclusion[Params, BFn, R, RFn](code: CodeHolder[RFn], scenarios: List[Scenario[Params, BFn, R, RFn]]) extends DecisionTreeNode[Params, BFn, R, RFn]

case class Decision[Params, BFn, R, RFn](because: List[CodeHolder[BFn]], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn], scenarioThatCausedNode: Scenario[Params, BFn, R, RFn]) extends DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios = yes.scenarios ++ no.scenarios
  def isTrue(bc: (BFn) => Boolean) = because.foldLeft(false)((acc, ch) => acc || bc(ch.fn))
}

trait MakeClosures[Params, BFn, R, RFn] {
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R

  def safe[T](block: => T): Either[Exception, T] = try Right(block) catch { case e: Exception => Left(e) }

  def makeBecauseClosure(params: Params): BecauseClosure
  def makeBecauseClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): BecauseClosure

  def makeResultClosure(params: Params): ResultClosure
  def makeResultClosure(scenarioWithParams: Scenario[Params, BFn, R, RFn]): ResultClosure

  def evaluateBecause(scenarioWithbecause: Scenario[Params, BFn, R, RFn], scenarioWithParams: Scenario[Params, BFn, R, RFn]): Boolean =
    makeBecauseClosure(scenarioWithParams)(scenarioWithbecause.
      because.getOrElse(throw new IllegalArgumentException(s"Scenario doesn't have because\scenarioWithbecause")).fn)
  def evaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = makeResultClosure(scenarioWithParams)(rfn)
  def safeEvaluateResult(rfn: RFn, scenarioWithParams: Scenario[Params, BFn, R, RFn]) = safe(makeResultClosure(scenarioWithParams)(rfn))
}

trait EvaluateTree[Params, BFn, R, RFn] extends MakeClosures[Params, BFn, R, RFn] {
  def root: DecisionTreeNode[Params, BFn, R, RFn]
  type DT = DecisionTreeAndExceptions[Params, BFn, R, RFn]
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]
  type Result = Either[Exception, R]

  def evaluate(scenarioWithParams: Scenario[Params, BFn, R, RFn]): R = evaluate(root, scenarioWithParams)

  protected def evaluate(root: DTN, scenarioWithParams: Scenario[Params, BFn, R, RFn]): R = {
    val bc = makeBecauseClosure(scenarioWithParams)
    val c = findConclusion(root, bc).code
    makeResultClosure(scenarioWithParams).apply(c.fn)
  }
  protected def evaluate(root: DTN, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(root, bc).code
    makeResultClosure(params).apply(c.fn)
  }

  def safeEvaluate(scenarioWithParams: Scenario[Params, BFn, R, RFn]) = safe(evaluate(scenarioWithParams))

  def findConclusion(root: DTN, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = {
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => c
      case d: Decision[Params, BFn, R, RFn] => d.isTrue(bc) match {
        case true => findConclusion(d.yes, bc)
        case false => findConclusion(d.no, bc)
      }
    }
  }

}

