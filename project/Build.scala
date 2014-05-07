import sbt._
import Keys._

object CddBuild extends Build {

  lazy val engine = Project(id = "engine", base = file("engine"))
  lazy val examples = Project(id = "examples", base = file("examples")).dependsOn(engine,cddjunit)
  lazy val htmlRendering = Project(id = "htmlRendering", base = file("htmlRendering")).dependsOn(engine)
  lazy val cddjunit = Project(id = "cddjunit", base = file("cddjunit")).dependsOn( engine,htmlRendering)
  lazy val tests = Project(id = "tests", base = file("tests")).dependsOn( engine, htmlRendering,cddjunit)
//  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests,cddjunit)
  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests, htmlRendering, cddjunit, examples)
}