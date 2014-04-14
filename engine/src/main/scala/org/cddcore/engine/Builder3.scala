package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens3[P1, P2, P3, R, B <: EngineNodeHolder[R, (P1, P2, P3) => R]] extends BuilderLens[R, (P1, P2, P3) => R, B] {
  val becauseL = Lens[EngineNode[R, (P1, P2, P3) => R], Option[CodeHolder[(P1, P2, P3) => Boolean]]](
    (b) => b match { case s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] => s.because },
    (b, bCodeHolder) => b match { case s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] => s.copyScenario(because = bCodeHolder) })
}

object Builder3 {
  def bl[P1, P2, P3, R]() = new BuilderLens3[P1, P2, P3, R, Builder3[P1, P2, P3, R]]
  def expectedToCode[P1, P2, P3, R]: Either[Class[_ <: Exception], R] => CodeHolder[(P1, P2, P3) => R] =
    (x) => new CodeHolder((p1, p2, p3) => x match { case Right(r) => r }, x.toString())
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2, P3) => Boolean]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val ch = CodeHolder[(P1, P2, P3) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.becauseL).set(thisObject, Some(ch))
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2, P3) => R]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val ch = CodeHolder[(P1, P2, P3) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.codeL).set(thisObject, Some(ch))
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2, P3), R]]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val chBecause = CodeHolder[(P1, P2, P3) => Boolean]((p1, p2, p3) => pf.splice.isDefinedAt((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2, P3) => R]((p1, p2, p3) => pf.splice.apply((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice

      val withBecause = l.currentNodeL.andThen(l.becauseL).set(thisObject, Some(chBecause))
      l.currentNodeL.andThen(l.codeL).set(withBecause, Some(chResult))
    }
  }
}

case class Builder3[P1, P2, P3, R](nodes: List[EngineNode[R, (P1, P2, P3) => R]] = List(new EngineDescription[R, (P1, P2, P3) => R])) extends Builder[R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R]] with BuilderWithModifyChildrenForBuild[R, (P1, P2, P3) => R] {
  val bl3 = new BuilderLens3[P1, P2, P3, R, Builder3[P1, P2, P3, R]]
  import bl3._

  def scenario(p1: P1, p2: P2, p3: P3, title: String = null) = nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => new Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]((p1, p2, p3), Option(title)) :: nodes)
  def because(because: (P1, P2, P3) => Boolean) = macro Builder3.becauseImpl[P1, P2, P3, R]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2, P3) => Boolean]) = currentNodeL.andThen(becauseL).set(this, Some(becauseHolder))
  def code(code: (P1, P2, P3) => R) = macro Builder3.codeImpl[P1, P2, P3, R]
  def matchWith(pf: PartialFunction[(P1, P2, P3), R]) = macro Builder3.matchWithImpl[P1, P2, P3, R]
  def copyNodes(nodes: List[EngineNode[R, (P1, P2, P3) => R]]) = new Builder3[P1, P2, P3, R](nodes)

}
trait EvaluateTree3[P1, P2, P3, R] extends EvaluateTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] {
  def makeBecauseClosure(params: (P1, P2, P3)): BecauseClosure = ((bfn) => bfn(params._1, params._2, params._3))
  def makeResultClosure(params: (P1, P2, P3)): ResultClosure = ((rfn) => rfn(params._1, params._2, params._3))
}

case class Engine3[P1, P2, P3, R](root: DecisionTreeNode[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R], rootIsDefault: Boolean = false) extends DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] with EvaluateTree3[P1, P2, P3, R] with Function3[P1, P2, P3, R] {
  def apply(p1: P1, p2: P2, p3: P3) = evaluate(root, (p1, p2, p3))
  val lens = new DecisionTreeLens3[P1, P2, P3, R]((r) => new Engine3(r))
  val expectedToCode: Either[Class[_ <: Exception], R] => CodeHolder[(P1, P2, P3) => R] = Builder3.expectedToCode[P1, P2, P3, R]
}
