package org.cddcore.engine

import scala.language.implicitConversions

sealed trait DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios: List[Scenario[Params, BFn, R, RFn]]
}

case class Conclusion[Params, BFn, R, RFn](scenarios: List[Scenario[Params, BFn, R, RFn]], code: CodeHolder[RFn]) extends DecisionTreeNode[Params, BFn, R, RFn]

case class Decision[Params, BFn, R, RFn](because: List[CodeHolder[BFn]], inputs: Params, yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn], scenarioThatCausedNode: Scenario[Params, BFn, R, RFn]) extends DecisionTreeNode[Params, BFn, R, RFn] {
  def scenarios = yes.scenarios ++ no.scenarios
}

trait EvaluateTree[Params, BFn, R, RFn] {
  type DTN = DecisionTreeNode[Params, BFn, R, RFn]
  type BecauseClosure = (BFn) => Boolean
  type ResultClosure = (RFn) => R
  type Result = Either[Exception, R]
  def makeBecauseClosure(params: Params): BecauseClosure
  def makeResultClosure(params: Params): ResultClosure
  def safe[T](block: => T): Either[Exception, T] = try Right(block) catch { case e: Exception => Left(e) }

  def evaluteBecause(b: BFn, params: Params): Boolean = makeBecauseClosure(params)(b)
  def evaluteResult(rfn: RFn, params: Params) = makeResultClosure(params)(rfn)

  def evaluate(root: DTN, params: Params): R = {
    val bc = makeBecauseClosure(params)
    val c = findConclusion(root, bc).code
    makeResultClosure(params).apply(c.fn)
  }

  def safeEvaluate(root: DTN, params: Params) = safe(evaluate(root, params))

  def findConclusion(root: DTN, bc: BecauseClosure): Conclusion[Params, BFn, R, RFn] = {
    root match {
      case c: Conclusion[Params, BFn, R, RFn] => c
      case d: Decision[Params, BFn, R, RFn] => d.because.foldLeft(false)((acc, ch) => acc || bc(ch.fn)) match {
        case true => findConclusion(d.yes, bc)
        case false => findConclusion(d.no, bc)
      }
    }
  } 
}




trait DecisionTree[Params, BFn, R, RFn] extends Engine[Params, BFn, R, RFn] with EvaluateTree[Params, BFn, R, RFn] {
  def root: DTN
}



