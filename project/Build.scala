import sbt._
import Keys._

object CddBuild extends Build {

  lazy val engine = Project(id = "engine", base = file("engine"))
  lazy val examples = Project(id = "examples", base = file("examples")).dependsOn(engine,cddjunit)
  lazy val tests = Project(id = "tests", base = file("tests")).dependsOn( engine)
  lazy val htmlRendering = Project(id = "htmlRendering", base = file("htmlRendering")).dependsOn( cddjunit,tests % "compile->compile;test->test")
  lazy val cddjunit = Project(id = "cddjunit", base = file("cddjunit")).dependsOn( engine)
//  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests,cddjunit)
  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests, htmlRendering, cddjunit, examples)
}