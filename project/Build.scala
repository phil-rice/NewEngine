import sbt._
import Keys._

object CddBuild extends Build {

  lazy val engine = Project(id = "engine", base = file("engine"))
  lazy val structure =Project(id = "structure", base = file("structure")).dependsOn(engine)
  lazy val htmlRendering = Project(id = "htmlRendering", base = file("htmlRendering")).dependsOn(engine)
  lazy val cddjunit = Project(id = "cddjunit", base = file("cddjunit")).dependsOn( engine,htmlRendering)
  lazy val examples = Project(id = "examples", base = file("examples")).dependsOn(engine,cddjunit, structure)
  lazy val tests = Project(id = "tests", base = file("tests")).dependsOn( engine, htmlRendering,cddjunit, structure)
//  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests,cddjunit)
  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests, htmlRendering, structure, cddjunit, examples)
}