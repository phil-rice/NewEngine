package org.cddcore.engine

object ExceptionScenarioPrinter {
  val fullScenario = false
  def apply[Params, BFn, R, RFn](s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = scenario2Str(s)
  def existingAndBeingAdded[Params, BFn, R, RFn](existing: Scenario[Params, BFn, R, RFn], s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) =
    s"Existing: ${existing.titleString}\nBeing Added: ${s.titleString}\nDetailed existing:\n${ldp(existing)}\nDetailed of being Added:\n${ldp(s)}"
  def full[Params, BFn, R, RFn](s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor) = s + "\nDetailed:\n  " + ldp(s.params)
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
class UndecidedException extends EngineException

class ScenarioException(msg: String, val scenario: Scenario[_, _, _, _], cause: Throwable = null) extends EngineException(msg, cause)

object DuplicateScenarioException {
  def apply[Params, BFn, R, RFn](scenario: Scenario[Params, BFn, R, RFn]) = new DuplicateScenarioException(s"Duplicate scenario $scenario", scenario)
}
class DuplicateScenarioException(msg: String, scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

object ScenarioConflictingWithDefaultException {
  def apply[R](actual: Either[Exception, R], s: Scenario[_, _, R, _])(implicit ldp: LoggerDisplayProcessor) =
    new ScenarioConflictingWithDefaultException(s"\nActual Result: ${actual}\nExpected ${s.expected.getOrElse("<N/A>")}\n ${ExceptionScenarioPrinter.full(s)}", actual, s)
}
class ScenarioConflictingWithDefaultException(msg: String, val actual: Either[Exception, _], scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

object NoExpectedException {
  def apply(scenario: Scenario[_, _, _, _], cause: Throwable = null)(implicit ldp: LoggerDisplayProcessor) = new NoExpectedException(s"No expected in ${ExceptionScenarioPrinter.full(scenario)}", scenario, cause)
}
class NoExpectedException(msg: String, scenario: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)

object ScenarioBecauseException {
  def apply(scenario: Scenario[_, _, _, _], cause: Throwable = null)(implicit ldp: LoggerDisplayProcessor) =
    new ScenarioBecauseException(s"No expected in ${ExceptionScenarioPrinter.full(scenario)}", scenario, cause)
}
class ScenarioBecauseException(msg: String, scenario: Scenario[_, _, _, _], cause: Throwable) extends ScenarioException(msg, scenario, cause)

object CameToWrongConclusionScenarioException {
  def apply(expected: Any, actual: Any, s: Scenario[_, _, _, _])(implicit ldp: LoggerDisplayProcessor) =
    new CameToWrongConclusionScenarioException(s"CDD Error: Scenario came to wrong result\nExpected\n$expected\nActual\n$actual\nScenario\n${ExceptionScenarioPrinter.full(s)}", expected, actual, s)

}
class CameToWrongConclusionScenarioException(msg: String, val expected: Any, val actual: Any, s: Scenario[_, _, _, _]) extends ScenarioException(msg, s)

object AssertionException {
  def apply(a: CodeHolder[_], s: Scenario[_, _, _, _])(implicit ldp: LoggerDisplayProcessor) =
    new AssertionException(s"Assertion failed\n${a.description}\n${ExceptionScenarioPrinter.full(s)}", a.fn, s)
}

class AssertionException(msg: String, val assertion: Any, scenario: Scenario[_, _, _, _]) extends ScenarioException(msg, scenario)

