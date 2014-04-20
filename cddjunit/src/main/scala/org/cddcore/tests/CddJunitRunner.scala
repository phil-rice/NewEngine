package org.cddcore.tests

import java.io.File
import java.lang.reflect.Field
import java.lang.reflect.Method
import java.util.IdentityHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.Array.canBuildFrom

import org.cddcore.engine.BuilderNodeHolder
import org.cddcore.engine.Engine
import org.cddcore.engine.Engine.test
import org.cddcore.engine.EngineFromTests
import org.cddcore.engine.ExceptionMap
import org.cddcore.engine.FoldingEngine
import org.cddcore.engine.LoggerDisplayProcessor
import org.cddcore.engine.MultipleScenarioExceptions
import org.cddcore.engine.Requirement
import org.cddcore.engine.Scenario
import org.cddcore.utilities.MapToUniqueName
import org.cddcore.utilities.Reflection
import org.cddcore.utilities.Strings
import org.junit.runner.Description
import org.junit.runner.Runner
import org.junit.runner.notification.Failure
import org.junit.runner.notification.RunNotifier

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
  val requirementToEngines = new IdentityHashMap[Requirement, Engine[_, _, _, _]]

  val reportableToDescription = new IdentityHashMap[Requirement, Description]()
  /** Descriptions should have unique names as JUnit has defined equals on it.  */
  def makeDescriptionfor(r: Requirement): Description = {
    names = names.add(r)
    val name = names(r)
    val description = Description.createSuiteDescription(name)
    reportableToDescription.put(r, description)
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
  var exceptionMap: ExceptionMap = new ExceptionMap()

  def title: String

  def addEngine(e: Engine[_, _, _, _]) {
    exceptionMap = exceptionMap ++ e.buildExceptions
    requirementToEngines.put(e.asRequirement, e)
    allEngines = e :: allEngines
    e match {
      case f: FoldingEngine[_, _, _, _, _] => for (e <- f.engines) addEngine(e)
      case _ =>
    }
  }
  for (e <- rootEngines)
    addEngine(e)

  lazy val getDescription = {
    val result = Description.createSuiteDescription(title);
    if (Engine.logging) println("Running\n" + rootEngines.mkString("\n---------------------\n"))
    for (engine <- rootEngines)
      addRequirement(result, engine.asRequirement)
    result
  }

  def addRequirement(d: Description, r: Requirement): Unit = {
    val childDescription = makeDescriptionfor(r)
    val name = names(r)
    d.addChild(childDescription)
    r match {
      case holder: BuilderNodeHolder[_, _] => for (c <- holder.nodes) addRequirement(childDescription, c)
      case _ =>
    }
  }
  def newEngine[Params, BFn, R, RFn](r: Requirement, e: Option[EngineFromTests[Params, BFn, R, RFn]]): Option[EngineFromTests[Params, BFn, R, RFn]] =
    requirementToEngines.get(r) match {
      case e: EngineFromTests[Params, BFn, R, RFn] => Some(e)
      case _ => e
    }

  def run(notifier: RunNotifier) = {
    for (e <- rootEngines) {
      val r = e.asRequirement
      runRequirement(notifier, newEngine(r, None), r)
    }
    if (Engine.logging) println("Done")
  }

  def runRequirement[Params, BFn, R, RFn](notifier: RunNotifier, e: Option[EngineFromTests[Params, BFn, R, RFn]], r: Requirement) {
    var description = reportableToDescription.get(r)
    if (Engine.logging) println(description)
    if (exceptionMap.contains(r)) {
      if (Engine.logging) println("          <<<<<<< Exception for " + r)
      notifier.fireTestStarted(description)
      exceptionMap(r) match {
        case e :: Nil =>
          notifier.fireTestFailure(new Failure(description, e))
        case listE =>
          notifier.fireTestFailure(new Failure(description, MultipleScenarioExceptions(listE)))
      }
    } else
      r match {
        case holder: BuilderNodeHolder[R, RFn] => {
          try {
            if (Engine.logging) println("          <<<<<<< Holder " + r)
            notifier.fireTestStarted(description)
            for (child <- holder.nodes)
              runRequirement(notifier, newEngine(holder, e), child)
            notifier.fireTestFinished(description)
          } catch {
            case e: Exception => notifier.fireTestFailure(new Failure(description, e))
          }
        }
        case s: Scenario[Params, BFn, R, RFn] => {
          notifier.fireTestStarted(description)
          try {
            val engine = e.getOrElse(throw new IllegalStateException)
            val validator = engine.evaluator.validator
            validator.checkCorrectValue(engine.evaluator, engine.tree, s)
            notifier.fireTestFinished(description)
          } catch {
            case e: Exception =>
              if (Engine.logging) println("          <<<<<<< Test Exception for " + r)
              notifier.fireTestFailure(new Failure(description, e))
          }
        }
      }
  }

}

class CddJunitRunner(val clazz: Class[_]) extends CddRunner {
  import org.cddcore.engine.Engine._

  def title = "CDD: " + clazz.getName
  lazy val instance = test { Reflection.instantiate(clazz) };

  lazy val methodRootEngines = clazz.getDeclaredMethods().filter((m) => returnTypeIsEngine(m)).map((m) => m.invoke(instance).asInstanceOf[Engine[_, _, _, _]])
  lazy val variableRootEngines = clazz.getFields().filter((f) => typeIsEngine(f)).map((f) => f.get(instance).asInstanceOf[Engine[_, _, _, _]])
  lazy val rootEngines = test { methodRootEngines ++ variableRootEngines }.sortBy((e) => e.asRequirement.textOrder).toList
  CddRunner.addToEngines(rootEngines.toList)

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

trait HasEngines {
  def engines: List[Engine[_, _, _, _]]
}

class CddContinuousIntegrationRunner(val clazz: Class[Any]) extends CddRunner {
  def title = "Constraint Driven Development"
  lazy val instance = Engine.test { Reflection.instantiate(clazz) }.asInstanceOf[HasEngines];
  lazy val rootEngines = instance.engines
}

trait CddContinuousIntegrationTest extends HasEngines {
  def engines: List[Engine[_, _, _, _]]
}