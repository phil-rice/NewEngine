package org.cddcore.tests

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import org.cddcore.engine._
import java.io.File
import org.junit.runner.Runner
import java.lang.reflect._
import org.junit.runner.Description
import org.cddcore.utilities._

object CddRunner {
  val separator = "\n#########\n"
  val userHome = System.getProperty("user.home");
  var directory = new File(userHome, ".cdd")
  val needToAddListener = new AtomicBoolean(true)
  val enginesInTest = new AtomicReference(List[Engine[_, _, _, _]]())
  def addToEngines(l: List[Engine[_, _, _, _]]) {
    while (true) {
      val original = enginesInTest.get()
      val next = original ::: l
      if (enginesInTest.compareAndSet(original, next))
        return
    }
  }
}

trait CddRunner extends Runner {
  def ldp: LoggerDisplayProcessor = implicitly[LoggerDisplayProcessor]
  def clazz: Class[_ <: Any]
  val rootEngines: List[Engine[_, _, _, _]]
  var allEngines = List[Engine[_, _, _, _]]()

  var reportableToDescription = Map[Requirement, Description]()
  /** Descriptions should have unique names as JUnit has defined equals on it.  */
  def makeDescriptionfor(r: Requirement): Description = {
    names = names.add(r)
    val name = names(r)
    val description = Description.createSuiteDescription(name)
    reportableToDescription = reportableToDescription + (r -> description)
    description
  }
  var names = new MapToUniqueName[Requirement]((r: Requirement, count: Int) => {
    val name = Strings.clean(r match {
      case t: Scenario[_, _, _, _] =>
        val result = t.titleString + " => " + ldp(t.expected.getOrElse(""))
        result
      case r: Requirement => r.title.getOrElse("")
      case _ => throw new IllegalStateException(r.getClass() + "/" + r)
    })
    val result = count match { case 1 => name; case _ => name + "_" + count }
    result
  })
  var exceptionMap: Map[Requirement, List[Exception]] = Map()

  def title: String

  def addEngine(e: Engine[_, _, _, _]) {
    exceptionMap = exceptionMap ++ e.buildExceptions
    allEngines = e :: allEngines
    e match {
      case f: FoldingEngine[_, _, _, _, _] =>
          f.engines
    }
  }
  for (e <- rootEngines)
    addEngine(e)

  lazy val getDescription = {
    val result = Description.createSuiteDescription(title);
    if (Engine.logging) println("Running\n" + rootEngines.mkString("\n---------------------\n"))
    for (engine <- rootEngines) {
      val engineRequirement = engine.asRequirement
    }
    result
  }

  def addRequirement(d: Description, r: Requirement): Description = {
    val name = names(r)
    val childDescription = makeDescriptionfor(r)
    d.addChild(childDescription)

    r match {
      case holder: BuilderNodeHolder[_, _] => for (c <- holder.nodes) addRequirement(childDescription, c)
      case _ =>
    }

//    if (Engine.logging) {
//      println(name)
//      println(engine)
//    }
//
//    engineDescription
  }

}

class CddJunitRunner(val clazz: Class[_]) extends CddRunner {
  import org.cddcore.engine.Engine._

  def title = "CDD: " + clazz.getName
  val instance = test { Reflection.instantiate(clazz) };

  val methodRootEnginesAndNames = clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m)).map((m) => (m.invoke(instance).asInstanceOf[Engine[_, _, _, _]], m.getName))
  val variableRootEnginesAndNames = clazz.getFields().filter((f) => typeIsEngine(f)).map((f) => (f.get(instance).asInstanceOf[Engine[_, _, _, _]], f.getName)).sortBy(_._2)
  val enginesToNameMap = Map(rootEnginesAndNames: _*)
  val rootEnginesAndNames = test { methodRootEnginesAndNames ++ variableRootEnginesAndNames }
  val rootEngines = rootEnginesAndNames.map(_._1)
  CddRunner.addToEngines(rootEngines.toList)

  if (logging) {
    println(clazz)
    for ((name, engine) <- rootEnginesAndNames)
      println("Engine: " + name)
  }

  def returnTypeIsEngine(m: Method): Boolean = {
    val rt = m.getReturnType()
    return isEngine(rt)
  }

  def typeIsEngine(f: Field): Boolean = {
    val rt = f.getType()
    return isEngine(rt)
  }

  def isEngine(rt: Class[_]): Boolean = {
    val c = classOf[Engine[_, _, _, _]]
    if (c.isAssignableFrom(rt))
      return true;
    for (t <- rt.getInterfaces())
      if (c.isAssignableFrom(t))
        return true;
    return false;
  }

}