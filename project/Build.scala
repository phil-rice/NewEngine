import sbt._
import Keys._

object CarersBuild extends Build {

  lazy val engine = Project(id = "engine", base = file("engine"))
  lazy val examples = Project(id = "examples", base = file("examples")).dependsOn(engine)
  lazy val tests = Project(id = "tests", base = file("tests")).dependsOn( engine)
  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests,examples)
}