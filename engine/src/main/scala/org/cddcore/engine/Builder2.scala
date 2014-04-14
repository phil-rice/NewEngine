package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens2[P1, P2, R, B <: EngineNodeHolder[R, (P1, P2) => R]] extends BuilderLens[R, (P1, P2) => R, B] {
  val becauseL = Lens[EngineNode[R, (P1, P2) => R], Option[CodeHolder[(P1, P2) => Boolean]]](
    (b) => b match { case s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] => s.because },
    (b, bCodeHolder) => b match { case s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] => s.copyScenario(because = bCodeHolder) })
}

object Builder2 {
  def bl[P1, P2, R]() = new BuilderLens2[P1, P2, R, Builder2[P1, P2, R]]
  def expectedToCode[P1, P2, R]: Either[Class[_ <: Exception], R] => CodeHolder[(P1, P2) => R] =
    (x) => new CodeHolder((p1, p2) => x match { case Right(r) => r }, x.toString())
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2) => Boolean]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R]()
      val ch = CodeHolder[(P1, P2) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.becauseL).set(thisObject, Some(ch))
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2) => R]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R]()
      val ch = CodeHolder[(P1, P2) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.codeL).set(thisObject, Some(ch))
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R]()
      val chBecause = CodeHolder[(P1, P2) => Boolean]((p1, p2) => pf.splice.isDefinedAt((p1, p2)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2) => R]((p1, p2) => pf.splice.apply((p1, p2)), c.literal(show(pf.tree)).splice)
      val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice

      val withBecause = l.currentNodeL.andThen(l.becauseL).set(thisObject, Some(chBecause))
      l.currentNodeL.andThen(l.codeL).set(withBecause, Some(chResult))
    }
  }
}
case class Builder2[P1, P2, R](nodes: List[EngineNode[R, (P1, P2) => R]] = List(new EngineDescription[R, (P1, P2) => R])) extends Builder[R, (P1, P2) => R, Builder2[P1, P2, R]] with BuilderWithModifyChildrenForBuild[R, (P1, P2) => R] {
  val bl2 = new BuilderLens2[P1, P2, R, Builder2[P1, P2, R]]
  import bl2._

  def code(code: (P1, P2) => R): Builder2[P1, P2, R] = macro Builder2.codeImpl[P1, P2, R]
  def because(because: (P1, P2) => Boolean): Builder2[P1, P2, R] = macro Builder2.becauseImpl[P1, P2, R]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2) => Boolean]) = currentNodeL.andThen(becauseL).set(this, Some(becauseHolder))
  def matchWith(pf: PartialFunction[(P1, P2), R]) = macro Builder2.matchWithImpl[P1, P2, R]
  def scenario(p1: P1, p2: P2, title: String = null) = nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => new Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]((p1, p2), title = Option(title)) :: nodes)
  def copyNodes(nodes: List[EngineNode[R, (P1, P2) => R]]) = new Builder2[P1, P2, R](nodes)

}

trait EvaluateTree2[P1, P2, R] extends EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] {
  def makeBecauseClosure(params: (P1, P2)): BecauseClosure = ((bfn) => bfn(params._1, params._2))
  def makeResultClosure(params: (P1, P2)): ResultClosure = ((rfn) => rfn(params._1, params._2))
}

case class Engine2[P1, P2, R](root: DecisionTreeNode[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R], rootIsDefault: Boolean = false) extends DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with EvaluateTree2[P1, P2, R] with Function2[P1, P2, R] {
  val lens = new DecisionTreeLens2[P1, P2, R]((r) => new Engine2(r))
  def apply(p1: P1, p2: P2) = evaluate(root, (p1, p2))
  val expectedToCode: Either[Class[_ <: Exception], R] => CodeHolder[(P1, P2) => R] = Builder2.expectedToCode
}
