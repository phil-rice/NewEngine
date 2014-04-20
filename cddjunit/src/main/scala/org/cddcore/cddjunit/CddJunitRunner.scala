package org.cddcore.cddjunit

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
import org.cddcore.engine.FoldingEngine
import org.cddcore.engine.Reportable
import org.cddcore.engine.Requirement
import org.cddcore.engine.Scenario
import org.cddcore.utilities.ExceptionMap
import org.cddcore.utilities.LoggerDisplayProcessor
import org.cddcore.utilities.MapToUniqueName
import org.cddcore.utilities.Reflection
import org.cddcore.utilities.Strings
import org.junit.runner.Description
import org.junit.runner.Runner
import org.junit.runner.notification.RunNotifier
import org.junit.runner.notification.Failure
import org.cddcore.engine.builder.Engine1
import org.cddcore.utilities.KeyedMap
import org.cddcore.utilities.KeyLike
import org.cddcore.engine.MultipleScenarioExceptions

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

trait EngineWalker {
  def walk()
}

trait CddRunner extends Runner {
  def ldp: LoggerDisplayProcessor = implicitly[LoggerDisplayProcessor]
  def clazz: Class[_ <: Any]
  val rootEngines: List[Engine[_, _, _, _]]
  var allEngines = List[Engine[_, _, _, _]]()
  var reportableToDescription = new KeyedMap[Description]()
  var exceptionMap: ExceptionMap = new ExceptionMap()
  /** Descriptions should have unique names as JUnit has defined equals on it.  This helps avoid subtle bugs*/
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

  def title: String

  def addEngine(e: Engine[_, _, _, _]) {
    exceptionMap = exceptionMap ++ e.buildExceptions
    allEngines = e :: allEngines
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
    if (Engine.logging) println(s"Adding requirement: $r")
    val childDescription = makeDescriptionfor(r)
    val name = names(r)
    d.addChild(childDescription)
    r match {
      case holder: BuilderNodeHolder[_, _] => for (c <- holder.nodes) addRequirement(childDescription, c)
      case _ =>
    }
  }

  def run(notifier: RunNotifier) = {
    import KeyLike._
    def fail(d: Description, e: Exception) = {
      if (Engine.logging) { println(s"Failed: d"); e.printStackTrace() }
      notifier.fireTestFailure(new Failure(d, e))
    }

    def runDescription(description: Description, middle: => Unit) {
      if (Engine.logging) println(s"Starting: $description")
      notifier.fireTestStarted(description);
      try {
        middle
        notifier.fireTestFinished(description)
        if (Engine.logging) println(s"Finishing: $description")
      } catch { case e: Exception => fail(description, e) }
    }

    def runReportable(r: Requirement, middle: => Unit) = {
      val description = reportableToDescription(r);
      if (description == null) {
        if (Engine.logging) println(s"No description found for $r")
        throw new IllegalStateException(s"No description found for $r")
      }
      exceptionMap.get(r) match {
        case Some(e :: Nil) => fail(description, e)
        case Some(eList) => fail(description, MultipleScenarioExceptions(eList))
        case _ => runDescription(description, middle)
      }
    }
    def runReportableAndChildren[Params, BFn, R, RFn](r: Requirement, e: EngineFromTests[Params, BFn, R, RFn]): Unit =
      runReportable(r, r match {
        case holder: BuilderNodeHolder[R, RFn] => for (child <- holder.nodes) runReportableAndChildren(child, e)
        case scenario: Scenario[Params, BFn, R, RFn] => import e._; evaluator.validator.checkCorrectValue(evaluator, tree, scenario)
      })
    def runEngine[Params, BFn, R, RFn, FullR](e: Engine[Params, BFn, R, RFn]): Unit = e match {
      case f: FoldingEngine[Params, BFn, R, RFn, FullR] => runReportable(f.asRequirement, for (e <- f.engines) runEngine(e))
      case e: EngineFromTests[Params, BFn, R, RFn] => runReportableAndChildren(e.asRequirement, e)
    }
    runDescription(getDescription, { for (e <- rootEngines) runEngine(e) })

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