package org.cddcore.examples

import org.cddcore.engine.Engine

object Example {

  case class A(name: Int)
  case class B(name: Int)

  def main(args: Array[String]) {

    Engine[A, String]().
      scenario(new A(0)).
      code((x) => "x").
      //      matchOn { case x => "x" }.
      scenario(new A(0))

//    Engine.folding[A, String, String]("", { _ + _ }).childEngine("someEngine")

    //      matchOn { case x: B => "x" }
    //      useCase("",
    //        Scenario(new A(0)),
    //        Scenario(new A(0)),
    //        Scenario(new A(0)))

    //    new Builder2[A, B, String]().
    //      scenario(new A(0), new B(1)).code((a, b) => "x").because { _ == _ }.
    //      matchOn { case (a, b) => "x" }
    //
    //  }
  }
}