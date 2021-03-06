package org.cddcore.engine
import org.cddcore.utilities._

object ExceptionScenarioPrinter {
  val fullScenario = false
  def apply[Params, BFn, R, RFn](s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = scenario2Str(s)
  def existingAndBeingAdded[Params, BFn, R, RFn](existing: List[Scenario[Params, BFn, R, RFn]], s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) =
    s"Existing: ${existing.map(scenario2Str(_)).mkString(",")}\nBeing Added: ${scenario2Str(s)}\nDetailed existing:\n${existing.map((x) => ldp(x)).mkString("\n")}\nDetailed of being Added:\n${ldp(s)}"
  def full[Params, BFn, R, RFn](s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = "Scenario:\n" + s + "\nParameters:\n" + ldp(s.params)
  def scenario2Str[Params, BFn, R, RFn](s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) =
    if (fullScenario)
      s.titleString
    else
      ldp(s.params)

}

class EngineException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this() = this("", null)
  def this(msg: String) = this(msg, null)
}
class CannotHaveChildEnginesWithoutFolderException extends EngineException

object UndecidedException {
  protected def params(ps: Any*)(implicit ldp: LoggerDisplayProcessor) =
    (ps.size match { case 0 => "Param: "; case _ => "\nParams:\n  " }) + ps.zipWithIndex.map { case (p, i) => s"Param${i + 1}: ${ldp(p)}" }.mkString("\n  ")
  def apply[P](p: P)(implicit ldp: LoggerDisplayProcessor) = new UndecidedException(params(p), p)
  def apply[P1, P2](p1: P1, p2: P2)(implicit ldp: LoggerDisplayProcessor) = new UndecidedException(params(p1, p2), (p1, p2))
  def apply[P1, P2, P3](p1: P1, p2: P2, p3: P3)(implicit ldp: LoggerDisplayProcessor) = new UndecidedException(params(p1, p2, p3), (p1, p2, p3))
}

class UndecidedException(msg: String, val params: Any) extends EngineException(msg)

object NeedScenarioException {
  def apply() = new NeedScenarioException("This operation needed a scenario")
}
class NeedScenarioException(msg: String) extends EngineException(msg)

class ScenarioException(msg: String, val scenario: Scenario[_, _, _, _], cause: Throwable = null) extends EngineException(msg, cause)

object DuplicateScenarioException {
  def apply[Params, BFn, R, RFn](scenario: Scenario[Params, BFn, R, RFn]) = new DuplicateScenarioException(s"Duplicate scenario $scenario", scenario)
}
class DuplicateScenarioException(msg: String, scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

object ScenarioConflictingWithDefaultAndNoBecauseException {
  def apply[R](lens: Lens[_, _], actual: Either[Exception, R], expected: Either[Exception, R], s: Scenario[_, _, R, _])(implicit ldp: LoggerDisplayProcessor) =
    new ScenarioConflictingWithDefaultAndNoBecauseException(s"\n$lens\nActual Result:\n${actual}\nExpected\n${expected}\n${ExceptionScenarioPrinter.full(s)}", lens, actual, s)
}
class ScenarioConflictingWithDefaultAndNoBecauseException(msg: String, val lens: Lens[_, _], val actual: Either[Exception, _], scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

//object ScenarioConflictException {
//  def apply(loggerDisplayProcessor: LoggerDisplayProcessor, existing: Scenario, beingAdded: Scenario, cause: Throwable = null) =
//    new ScenarioConflictException(s"Cannot differentiate based on:\n ${beingAdded.becauseString}" +
//      s"\n${ExceptionScenarioPrinter.existingAndBeingAdded(loggerDisplayProcessor, existing, beingAdded)}", existing, beingAdded, cause)
//}
class ScenarioConflictException(msg: String, val existing: List[Scenario[_, _, _, _]], val beingAdded: Scenario[_, _, _, _], cause: Throwable = null) extends ScenarioException(msg, beingAdded, cause)
//
object ScenarioConflictingWithoutBecauseException {
  def apply[Params, BFn, R, RFn](lens: Lens[_, _], expected: Either[Exception, R], actual: Either[Exception, R], existing: List[Scenario[Params, BFn, R, RFn]], beingAdded: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = {
    new ScenarioConflictingWithoutBecauseException(s"\nCame to wrong conclusion: ${actual}\nInstead of ${expected}\n$lens\n${ExceptionScenarioPrinter.existingAndBeingAdded(existing, beingAdded)}", expected, actual, existing, beingAdded)
  }
}
class ScenarioConflictingWithoutBecauseException(msg: String, val expected: Either[Exception, _], val actual: Either[Exception, _], existing: List[Scenario[_, _, _, _]], beingAdded: Scenario[_, _, _, _], cause: Throwable = null) extends ScenarioConflictException(msg, existing, beingAdded, cause)

object ScenarioConflictAndBecauseNotAdequateException {
  def apply[Params, BFn, R, RFn](lens: Lens[_, _], expected: Either[Exception, R], actual: Either[Exception, R], existing: List[Scenario[Params, BFn, R, RFn]], beingAdded: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = {
    val becauseString = "\n" +
      "The because is valid in these other scenarios, which the engine thinks are similar.\n" +
      "The Engine doesn't have enough information to decide what to do\n" +
      "To resolve this you could improve the because clause in this scenario to differentiate it from the listed scenarios\n" +
      "If you understand how the engine is constructed, you could change scenario priorities, but often it is better to refine the because clause\n" +
      "The because clause is " + beingAdded.because.getOrElse(throw new IllegalStateException).description
    new ScenarioConflictAndBecauseNotAdequateException(s"$becauseString\nCame to wrong conclusion: ${actual}\nInstead of ${expected}\n$lens\n${ExceptionScenarioPrinter.existingAndBeingAdded(existing, beingAdded)}", expected, actual, existing, beingAdded)
  }
}
class ScenarioConflictAndBecauseNotAdequateException(msg: String, val expected: Either[Exception, _], val actual: Either[Exception, _], existing: List[Scenario[_, _, _, _]], beingAdded: Scenario[_, _, _, _], cause: Throwable = null)
  extends ScenarioConflictException(msg, existing, beingAdded, cause)

object NoExpectedException {
  def apply(scenario: Scenario[_, _, _, _], cause: Throwable = null)(implicit ldp: LoggerDisplayProcessor) =
    new NoExpectedException(s"No expected in ${ExceptionScenarioPrinter.full(scenario)}", scenario, cause)
}
class NoExpectedException(msg: String, scenario: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)

object CodeDoesntProduceExpectedException {
  def apply(scenario: Scenario[_, _, _, _], actual: Either[Exception, _], cause: Throwable = null)(implicit ldp: LoggerDisplayProcessor) =
    new CodeDoesntProduceExpectedException(s"Code block doesn't produce expected.\nExpected result: ${scenario.expected.get}\nActual result: $actual\nCode:\n${scenario.code.get}\n${ExceptionScenarioPrinter.full(scenario)}", scenario, actual, cause)
}
class CodeDoesntProduceExpectedException(msg: String, scenario: Scenario[_, _, _, _], val actual: Either[Exception, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)


object ScenarioBecauseException {
  def apply(scenario: Scenario[_, _, _, _], cause: Throwable = null)(implicit ldp: LoggerDisplayProcessor) =
    new ScenarioBecauseException(s"Because is not true. Because is\n${scenario.because.getOrElse(throw new IllegalStateException).description}\n${ExceptionScenarioPrinter.full(scenario)}", scenario, cause)
}
class ScenarioBecauseException(msg: String, scenario: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)

object CameToWrongConclusionScenarioException {
  def apply(expected: Any, actual: Any, s: Scenario[_, _, _, _], cause: Throwable)(implicit ldp: LoggerDisplayProcessor) =
    new CameToWrongConclusionScenarioException(s"CDD Error: Scenario came to wrong result\nExpected\n$expected\nActual\n$actual\nScenario\n${ExceptionScenarioPrinter.full(s)}", expected, actual, s, cause)

}
class CameToWrongConclusionScenarioException(msg: String, val expected: Any, val actual: Any, s: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, s, cause)

object AssertionException {
  def apply(a: CodeHolder[_], s: Scenario[_, _, _, _])(implicit ldp: LoggerDisplayProcessor) =
    new AssertionException(s"Assertion failed\n${a.description}\n${ExceptionScenarioPrinter.full(s)}", a.fn, s)
}

class AssertionException(msg: String, val assertion: Any, scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

object CannotDefineExpectedTwiceException {
  def apply(original: Either[Exception, _], beingAdded: Either[Exception, _]) = new CannotDefineExpectedTwiceException(s"Original${original}\nBeing Added $beingAdded ", original, beingAdded);
}
class CannotDefineExpectedTwiceException(msg: String, val original: Either[Exception, _], val beingAdded: Either[Exception, _]) extends EngineException(msg, null)

class CannotDefineBecauseTwiceException(msg: String, val original: CodeHolder[_], val beingAdded: CodeHolder[_]) extends EngineException(msg, null)
object CannotDefineBecauseTwiceException {
  def apply(original: CodeHolder[_], beingAdded: CodeHolder[_]) = new CannotDefineBecauseTwiceException(s"Original:\n${original}\nBeingAdded\n${beingAdded}", original, beingAdded);
}
object CannotDefineCodeTwiceException {
  def apply(original: CodeHolder[_], beingAdded: CodeHolder[_]) = new CannotDefineCodeTwiceException(s"Original:\n${original}\nBeingAdded\n${beingAdded}", original, beingAdded);
}
class CannotDefineCodeTwiceException(msg: String, val original: CodeHolder[_], val beingAdded: CodeHolder[_]) extends EngineException(msg, null)

object CannotDefineTitleTwiceException {
  def apply(original: String, beingAdded: String) =
    new CannotDefineTitleTwiceException(s"Original ${original}\nBeing Added $beingAdded ", original, beingAdded);
}
class CannotDefineTitleTwiceException(msg: String, val original: String, val beingAdded: String) extends EngineException(msg, null)

object CannotDefineDescriptionTwiceException {
  def apply(original: String, beingAdded: String) = new CannotDefineDescriptionTwiceException(s"Original ${original}\nBeing Added $beingAdded ", original, beingAdded);
}
class CannotDefineDescriptionTwiceException(msg: String, val original: String, val beingAdded: String) extends EngineException(msg, null)

object CannotDefinePriorityTwiceException {
  def apply(original: Int, beingAdded: Int) = new CannotDefinePriorityTwiceException(s"Original ${original} Being Added $beingAdded ", original, beingAdded);
}
class CannotDefinePriorityTwiceException(msg: String, val original: Int, val beingAdded: Int) extends EngineException(msg, null)
object CannotSendNoneToOptionLens {
  def apply(lens: OptionLens[_, _]) =
    lens.description match {
      case Some(d) => new CannotSendNoneToOptionLens(s"Lens: $d, ${lens}")
      case _ => new CannotSendNoneToOptionLens(s"Lens: ${lens}")
    }
}
class CannotSendNoneToOptionLens(msg: String) extends EngineException(msg)
class CannotHaveFoldingEngineWithoutChildEnginesException extends EngineException

object BecauseClauseScenarioException {
  def apply(scenario: Scenario[_, _, _, _], cause: Throwable)(implicit ldp: LoggerDisplayProcessor) =
    throw new BecauseClauseScenarioException((s"Threw exception evaluating because ${scenario.because.getOrElse(throw new IllegalStateException).description} \n${ExceptionScenarioPrinter.full(scenario)}"), scenario, cause)
}

class BecauseClauseScenarioException(msg: String, scenario: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)

object BecauseClauseException {
  def apply(params: Any, cause: Throwable)(implicit ldp: LoggerDisplayProcessor) =
    throw new BecauseClauseException(s"Threw exception in a because clause", params, cause)
}

class BecauseClauseException(msg: String, val params: Any, cause: Throwable) extends EngineException(msg, cause)

object ScenarioCausingProblemWithOrRuleException {
  def apply(scenariosThatWouldBeBroken: List[Scenario[_, _, _, _]], beingAdded: Scenario[_, _, _, _])(implicit ldp: LoggerDisplayProcessor) = {
    val msg = "The scenario you added already came to the correct conclusion. \n" +
      "As well as that it had a because clause, and if the because clause was added, other scenario(s) that as already been added would now come to the wrong conclusion\n" +
      s"Scenario being added:\n${ExceptionScenarioPrinter.full(beingAdded)}\n" +
      "\n------------------------------------------------------------------------------------\n" +
      "Scenarios that would be broken" +
      scenariosThatWouldBeBroken.map((s) => ExceptionScenarioPrinter.full(s)).mkString("\n------------------------------------------------------------------------------------\n")
    throw new ScenarioCausingProblemWithOrRuleException(msg, scenariosThatWouldBeBroken, beingAdded);
  }
}
class ScenarioCausingProblemWithOrRuleException(msg: String,
  val scenariosThatWouldBeBroken: List[Scenario[_, _, _, _]],
  beingAdded: Scenario[_, _, _, _]) extends ScenarioConflictException(msg, scenariosThatWouldBeBroken, beingAdded)

object MultipleScenarioExceptions {
  def apply(list: List[Exception])(implicit ldp: LoggerDisplayProcessor) = {
    val reversed = list.reverse
    val msg = s"Multiple Exceptions. There is a good chance that only the first one is 'real' the others may be caused as a consequence of it\n\n${list.mkString("\n------------------------------------------------------------------------------------\n")}\n" +
      "\n\nOnly first stack trace shown\n"
    new MultipleScenarioExceptions(msg, reversed)
  }
}
class MultipleScenarioExceptions(msg: String, val list: List[Exception]) extends EngineException(msg, list.head)

