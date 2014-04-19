import sbt._
import Keys._

object CarersBuild extends Build {

  lazy val engine = Project(id = "engine", base = file("engine"))
  lazy val junit = Project(id = "junit", base = file("junit")).dependsOn( engine)
  lazy val examples = Project(id = "examples", base = file("examples")).dependsOn(engine,junit)
  lazy val tests = Project(id = "tests", base = file("tests")).dependsOn( engine)
  lazy val root = Project(id = "root", base = file(".")).aggregate( engine, tests,junit, examples)
}