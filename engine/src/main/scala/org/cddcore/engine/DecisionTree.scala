package org.cddcore.engine

import scala.language.implicitConversions

trait DecisionTreeLens[Params, BFn, R, RFn] {
  def creator: (DecisionTreeNode[Params, BFn, R, RFn]) => DecisionTree[Params, BFn, R, RFn];
  def rootL = Lens[DecisionTree[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (tree) => tree.root,
    (tree, root) => creator(root))
  def toDecisionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Decision[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Decision[Params, BFn, R, RFn]],
    (d, x) => x)
  def toConclusionL = Lens[DecisionTreeNode[Params, BFn, R, RFn], Conclusion[Params, BFn, R, RFn]](
    (d) => d.asInstanceOf[Conclusion[Params, BFn, R, RFn]],
    (d, x) => x)
  def yesL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.yes,
    (d, x) => d.copy(yes = x))
  def noL = Lens[Decision[Params, BFn, R, RFn], DecisionTreeNode[Params, BFn, R, RFn]](
    (d) => d.no,
    (d, x) => d.copy(no = x))
}
class DecisionTreeLens1[P, R](val creator: (DecisionTreeNode[P, (P) => Boolean, R, (P) => R]) => DecisionTree[P, (P) => Boolean, R, (P) => R]) extends DecisionTreeLens[P, (P) => Boolean, R, (P) => R]
class DecisionTreeLens2[P1, P2, R](val creator: (DecisionTreeNode[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]) => DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]) extends DecisionTreeLens[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]
class DecisionTreeLens3[P1, P2, P3, R](val creator: (DecisionTreeNode[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]) => DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]) extends DecisionTreeLens[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]

trait DecisionTree[Params, BFn, R, RFn] extends EvaluateTree[Params, BFn, R, RFn] with Engine[Params, BFn, R, RFn] {
  def root: DTN
  def rootIsDefault: Boolean
  def expectedToCode: (Either[Class[_ <: Exception], R]) => CodeHolder[RFn]
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
}

sealed trait DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios: List[Scenario[Params, BFn, R, RFn]]
}

case class Conclusion[Params, BFn, R, RFn](scenarios: List[Scenario[Params, BFn, R, RFn]], code: CodeHolder[RFn]) extends DecisionTreeNode[Params, BFn, R, RFn]

case class Decision[Params, BFn, R, RFn](because: List[CodeHolder[BFn]], inputs: Params, yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn], scenarioThatCausedNode: Scenario[Params, BFn, R, RFn]) extends DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios = yes.scenarios ++ no.scenarios
  def isTrue(bc: (BFn) => Boolean) = because.foldLeft(false)((acc, ch) => acc || bc(ch.fn))
}

trait EvaluateTree[Params, BFn, R, RFn] {
  def root: DecisionTreeNode[Params, BFn, R, RFn]
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R
  type Result = Either[Exception, R]
  def makeBecauseClosure(params: Params): BecauseClosure
  def makeResultClosure(params: Params): ResultClosure
  def safe[T](block: => T): Either[Exception, T] = try Right(block) catch { case e: Exception => Left(e) }

  def evaluteBecause(b: BFn, params: Params): Boolean = makeBecauseClosure(params)(b)
  def evaluteResult(rfn: RFn, params: Params) = makeResultClosure(params)(rfn)

  def evaluate(params: Params): R = evaluate(root, params)
  def evaluate(root: DTN, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(root, bc).code
    makeResultClosure(params).apply(c.fn)
  }

  def safeEvaluate(root: DTN, params: Params) = safe(evaluate(root, params))

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

