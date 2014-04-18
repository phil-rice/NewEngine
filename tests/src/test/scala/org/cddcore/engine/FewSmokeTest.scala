package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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
    assertEquals(builder.nodes.head, engine.asRequirement)
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
    assertEquals(builder.nodes.head, engine.asRequirement)
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
    assertEquals(builder.nodes.head, engine.asRequirement)
    assertEquals(List("love - all", "zero"), engine(0, 0, 0))
  }

  //  it should "allow the match with syntax" in {
  //    val builder = Engine[Int, Int, String]().
  //      title("engine").description("description").
  //      useCase("useCase1").
  //      matchWith { case (0, 0) => "love - all" }
  //
  //    val engine = builder.build
  //    assertEquals(builder, engine.asRequirement)
  //    assertEquals("love - all", engine(0, 0))
  //
  //  }
}