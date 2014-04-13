package org.cddcore.engine

import org.scalatest._
import bsh.Node
import org.scalatest.FlatSpecLike

trait AssertEquals {
  def assertEquals[T1, T2](expected: T1, actual: T2, prefix: String = "") {
    def msg = prefix + "\nExpected\n" + expected + "\nActual\n" + actual
    if (expected.isInstanceOf[String] & actual.isInstanceOf[String]) {
      val expectedString = expected.asInstanceOf[String];
      val actualString = actual.asInstanceOf[String];
      if (expectedString == actualString)
        return ;
      val s: Traversable[Tuple2[Char, Char]] = for ((ce, ca) <- (expectedString, actualString).zipped) yield (ce, ca)
      for ((t, i) <- s.toList.zipWithIndex) {
        if (t._1 != t._2) {
          val expectedMax = Math.min(i + 10, expectedString.length() - 1)
          val actualMax = Math.min(i + 10, actualString.length() - 1)
          Matchers.fail("First fail at " + i + " Expected: [" + expectedString.substring(i, expectedMax) + "] Actual: [ " + actualString.substring(i, actualMax) + "]\n" + msg)
        }
      }
      expectedString.length() - actualString.length() match {
        case x if x < 0 => Matchers.fail(s"Actual ran over end at ${expectedString.length}\n" + msg)
        case x if x > 0 => Matchers.fail(s"Actual fell short end at ${actualString.length}\n" + msg)
      }

    }
    if (expected != actual)
      assert(expected == actual, msg)
  }

  def assertEquals[T1, T2](prefix: String, expected: T1, actual: T2) {
    assert(expected == actual, prefix + "\nExpected\n" + expected + "\nActual\n" + actual)
  }
}

trait AbstractTest extends FlatSpecLike with Matchers with AssertEquals

trait BuilderBeingTested[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] {
  private var builder: B = initializeBuilder()
  def update(fn: (B) => B): B = { val result = fn(builder); builder = result; result }
  def scenario(seed: Int, title: String = null) = builder = scenarioImpl(params(seed), title)
  def code(seed: Int) = update(_.codeHandler(resultCodeHolder(seed)))
  def currentBuilder: B = builder
  def initializeBuilder(nodes: List[EngineNode[R, RFn]] = List(new EngineDescription[R, RFn])): B
  protected def scenarioImpl(params: Params, title: String): B
  protected def scenarioObject(params: Params): Scenario[Params, BFn, R, RFn]
  protected def resultCodeHolder(seed: Int): CodeHolder[RFn]

  protected def buildImpl(b: B): E;
  def defaultRoot: DecisionTreeNode[Params, BFn, R, RFn]
  def build: E = buildImpl(builder)
  def resetBuilder = builder = initializeBuilder()
  def params(seed: Int): Params
  def result(seed: Int): R
}

trait BuilderTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends AbstractTest with BeforeAndAfter with BuilderBeingTested[Params, BFn, R, RFn, B, E] {
  before {
    resetBuilder
  }
  def builderName: String
}

trait StringStringTest {
  def builderName: String = "Builder1[String,String]"
  def params(seed: Int): String = s"Aseed.toString"
  def result(seed: Int): String = s"Result($seed)"
}

trait StringStringStringTest {
  def builderName: String = "Builder2[String,String, String]"
  def params(seed: Int) = (s"A$seed", s"B$seed")
  def result(seed: Int) = seed.toString
}
trait StringStringStringStringTest {
  def builderName: String = "Builder3[String,String,String,String]"
  def params(seed: Int) = (s"A$seed", s"B$seed", s"c$seed")
  def result(seed: Int) = seed.toString
}

trait StringIntTest {
  def builderName: String = "Builder[String,Int]"
  def params(seed: Int): String = seed.toString
  def result(seed: Int): Int = seed
}

trait Builder1Test[P, R] extends BuilderTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] {
  def initializeBuilder(nodes: List[EngineNode[R, (P) => R]]) = new Builder1[P, R](nodes)
  def scenarioImpl(p: P, title: String) = update(_.scenario(p, title))
  protected def resultCodeHolder(seed: Int) = new CodeHolder((p) => result(seed), s"(p)=>result$seed")
  protected def scenarioObject(p: P) = Scenario[P, (P) => Boolean, R, (P) => R](p)

  protected def buildImpl(b: Builder1[P, R]) = BuildEngine.build1(currentBuilder)
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode1[P, R])
}

trait Builder2Test[P1, P2, R] extends BuilderTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] {
  def initializeBuilder(nodes: List[EngineNode[R, (P1, P2) => R]]) = new Builder2[P1, P2, R](nodes)
  protected def scenarioImpl(params: (P1, P2), title: String) = { val (p1, p2) = params; update(_.scenario(p1, p2, title)) }
  protected def resultCodeHolder(seed: Int) = new CodeHolder((p1, p2) => result(seed), s"(p1,p2)=>result$seed")
  protected def scenarioObject(p: (P1, P2)) = Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](p)
  protected def buildImpl(b: Builder2[P1, P2, R]) = BuildEngine.build2(currentBuilder)
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode2[P1, P2, R])
}

trait Builder3Test[P1, P2, P3, R] extends BuilderTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] {
  def initializeBuilder(nodes: List[EngineNode[R, (P1, P2, P3) => R]]) = new Builder3[P1, P2, P3, R](nodes)
  protected def scenarioImpl(params: (P1, P2, P3), title: String) = { val (p1, p2, p3) = params; update(_.scenario(p1, p2, p3, title)) }
  protected def resultCodeHolder(seed: Int) = new CodeHolder((p1, p2, p3) => result(seed), s"(p1: P1, p2: P2, p3: P3) => Builder3Test.this.result($seed)")
  protected def scenarioObject(p: (P1, P2, P3)) = Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](p)
  protected def buildImpl(b: Builder3[P1, P2, P3, R]) = BuildEngine.build3(currentBuilder)
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode3[P1, P2, P3, R])
}