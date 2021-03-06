package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._

@RunWith(classOf[JUnitRunner])
class FewSmokeTest extends AbstractTest {
  val m = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "fourty")

  "An Engine1" should "be constructable from the Engine object" in {
    val builder = Engine[(Int, Int), String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario((0, 0)).expected("love - all")
    val engine = builder.build
    assertEquals(builder.nodes.head, engine.asRequirement)
    assertEquals("love - all", engine(0, 0))
  }
  "An Engine2" should "be constructable from the Engine object" in {
    val builder = Engine[Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0).expected("love - all")
    val engine = builder.build
    assertEquals(builder.nodes.head, engine.asRequirement)
    assertEquals("love - all", engine(0, 0))
  }
  "An Engine3" should "be constructable from the Engine object" in {
    val builder = Engine[Int, Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0, 99).expected("love - all")
    val engine = builder.build
    assertEquals(builder.nodes.head, engine.asRequirement)
    assertEquals("love - all", engine(0, 0, 99))
  }

  "A Folding Engine1" should "be constructable from the Engine object" in {
    val builder = Engine.foldList[(Int, Int), String].
      title("engine").description("description").
      childEngine("ce1").
      scenario((0, 0)).expected("love - all").
      childEngine("ce2").
      scenario((0, 0)).expected("zero")
    val engine = builder.build
    assertEquals(List("love - all", "zero"), engine(0, 0))
  }

  "A Folding Engine2" should "be constructable from the Engine object" in {
    val builder = Engine.foldList[Int, Int, String].
      title("engine").description("description").
      childEngine("ce1").
      scenario(0, 0).expected("love - all").
      childEngine("ce2").
      scenario(0, 0).expected("zero")
    val engine = builder.build
    assertEquals(List("love - all", "zero"), engine(0, 0))
  }
  "A Folding Engine3" should "be constructable from the Engine object" in {
    val builder = Engine.foldList[Int, Int, Int, String].
      title("engine").description("description").
      childEngine("ce1").
      scenario(0, 0, 0).expected("love - all").
      childEngine("ce2").
      scenario(0, 0, 0).expected("zero")
    val engine = builder.build
    assertEquals(List("love - all", "zero"), engine(0, 0, 0))
  }

  "An engine" should "have it's scenarios in text order" in {
    val engine = Engine[Int, Int]().
      scenario(0).expected(0).
      scenario(1).expected(1).code { _ + 0 }.priority(1).
      scenario(2).expected(2).
      build
    assertEquals(List(0, 1, 2), engine.asRequirement.scenarios.map(_.params).toList)
  }
  it should "remember an exception in the because, even if a later exception is thrown" in {
    val engine = Engine.test {
      Engine[Int, Int]().
        scenario(0).expected(0).code { _ + 0 }.
        scenario(1).because((x) => throw new RuntimeException).
        build
    }
    val s1 = engine.asRequirement.scenarios(1)
    val actual = engine.buildExceptions.map.mapValues((list) => list.map(_.getClass()))
    assertEquals(Map(s1.textOrder -> List(
      classOf[NoExpectedException],
      classOf[BecauseClauseScenarioException])), actual)
  }

  "An engine1" should "allow the match on syntax" in {
    val builder = Engine[Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0).expected("love - all").matchOn { case (0) => "love - all" }.
      scenario(1).expected("fifteen - all").matchOn { case (1) => "fifteen - all" }

    val engine = builder.build
    assertEquals("love - all", engine(0))
    assertEquals("fifteen - all", engine(1))

  }
  "An engine2" should "allow the match on syntax" in {
    val builder = Engine[Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0).expected("love - all").matchOn { case (0, 0) => "love - all" }.
      scenario(1, 1).expected("fifteen - all").matchOn { case (1, 1) => "fifteen - all" }

    val engine = builder.build
    assertEquals("love - all", engine(0, 0))
    assertEquals("fifteen - all", engine(1, 1))

  }
  "An engine3" should "allow the match on syntax" in {
    val builder = Engine[Int, Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0, 0).expected("love - all").matchOn { case (0, 0, 0) => "love - all" }.
      scenario(1, 1, 1).expected("fifteen - all").matchOn { case (1, 1, 1) => "fifteen - all" }

    val engine = builder.build
    assertEquals("love - all", engine(0, 0, 0))
    assertEquals("fifteen - all", engine(1, 1, 1))

  }
}