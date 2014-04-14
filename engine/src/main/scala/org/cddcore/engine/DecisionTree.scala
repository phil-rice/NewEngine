package org.cddcore.engine

import scala.language.implicitConversions

trait DecisionTreeLens[Params, BFn, R, RFn] {
  def creator: (DecisionTreeNode[Params, BFn, R, RFn]) => DecisionTree[Params, BFn, R, RFn];
  def rootL = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (tree) => tree.root,
    (tree, root) => creator(root),
    Some("rootL"))
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

trait DecisionTree[Params, BFn, R, RFn] extends EvaluateTree[Params, BFn, R, RFn]
  with Engine[Params, BFn, R, RFn] {
  def root: DTN
  def rootIsDefault: Boolean
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  val lens: DecisionTreeLens[Params, BFn, R, RFn]

  import lens._

  type LensToNode = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]]

  def findParentLens(bc: BecauseClosure) = findParentLensPrim(root, bc, rootL, None)

  protected def addStep(node: Decision[Params, BFn, R, RFn], bc: BecauseClosure, lensToNode: LensToNode) = node.isTrue(bc) match {
    case true => lensToNode.andThen(toDecisionL).andThen(yesL)
    case false => lensToNode.andThen(toDecisionL).andThen(noL)
  }

  protected def findParentLensPrim(root: DTN, bc: BecauseClosure, parentSoFar: LensToNode, parentWasTrue: Option[Boolean]): Option[(LensToNode, Boolean)] = {
    (parentWasTrue, root) match {
      case (None, c: Conclusion[Params, BFn, R, RFn]) => None //There is no parent
      case (Some(pWasTrue), c: Conclusion[Params, BFn, R, RFn]) => Some((parentSoFar, pWasTrue))
      case (Some(pWasTrue), d: Decision[Params, BFn, R, RFn]) =>
        val toHere = pWasTrue match {
          case true => parentSoFar.andThen(toDecisionL).andThen(yesL)
          case false => parentSoFar.andThen(toDecisionL).andThen(noL)
        }
        d.isTrue(bc) match {
          case true => findParentLensPrim(d.yes, bc, toHere, Some(true))
          case false => findParentLensPrim(d.yes, bc, toHere, Some(false))
        }
    }
  }
  def findLensToConclusion(bc: BecauseClosure): Lens[DT, DTN] = findLensToConclusion(root, bc, rootL)

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
  def makeResultClosure(params: Params): ResultClosure
  def evaluteBecause(b: BFn, params: Params): Boolean = makeBecauseClosure(params)(b)
  def evaluteResult(rfn: RFn, params: Params) = makeResultClosure(params)(rfn)
  def safeEvaluteResult(rfn: RFn, params: Params) = safe(makeResultClosure(params)(rfn))
}

trait EvaluateTree[Params, BFn, R, RFn] extends MakeClosures[Params, BFn, R, RFn] {
  def root: DecisionTreeNode[Params, BFn, R, RFn]
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]
  type Result = Either[Exception, R]

  def evaluate(params: Params): R = evaluate(root, params)
  protected def evaluate(root: DTN, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(root, bc).code
    makeResultClosure(params).apply(c.fn)
  }


  def safeEvaluate(params: Params) = safe(evaluate(params))

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

