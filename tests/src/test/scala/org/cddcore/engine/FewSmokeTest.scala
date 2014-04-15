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
    assertEquals(builder, engine.requirements)
    assertEquals("love - all", engine(0, 0))
  }
  "An Engine2" should "be constructable from the Engine object" in {
    val builder = Engine[Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0).expected("love - all")
    val engine = builder.build
    assertEquals(builder, engine.requirements)
    assertEquals("love - all", engine(0, 0))
  }
  "An Engine3" should "be constructable from the Engine object" in {
    val builder = Engine[Int, Int, Int, String]().
      title("engine").description("description").
      useCase("useCase1").
      scenario(0, 0, 99).expected("love - all")
    val engine = builder.build
    assertEquals(builder, engine.requirements)
    assertEquals("love - all", engine(0, 0, 99))
  }

  //  it should "allow the match with syntax" in {
  //    val builder = Engine[Int, Int, String]().
  //      title("engine").description("description").
  //      useCase("useCase1").
  //      matchWith { case (0, 0) => "love - all" }
  //
  //    val engine = builder.build
  //    assertEquals(builder, engine.requirements)
  //    assertEquals("love - all", engine(0, 0))
  //
  //  }
}