package org.cddcore.engine

import org.apache.log4j.Logger
import org.apache.log4j.Level

trait LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor): String
}

case class ClassFunction[C, T](clazz: Class[C], fn: (LoggerDisplayProcessor, C) => T) {
  def apply(loggerDisplayProcessor: LoggerDisplayProcessor, c: C) = fn(loggerDisplayProcessor, c)
}

case class ClassFunctionList[T](list: List[ClassFunction[_, T]] = List()) {
  def apply[C](ldp: LoggerDisplayProcessor, c: C) =
    list.collectFirst({ case ClassFunction(clazz, f) if clazz.isAssignableFrom(c.getClass) => f.asInstanceOf[(LoggerDisplayProcessor, C) => T](ldp, c.asInstanceOf[C]) })
  def getOrElse[C](ldp: LoggerDisplayProcessor, c: C, default: => T): T =
    apply(ldp, c).getOrElse(default)

}

trait LoggerDisplayProcessor extends Function[Any, String] {
  def displayMap: ClassFunctionList[String]
  def apply(a: Any): String =
    displayMap.getOrElse(this, a,
      a match {
        case d: LoggerDisplay => d.loggerDisplay(this);
        case a => a.toString
      })
}
case class SimpleLoggerDisplayProcessor(displayMap: ClassFunctionList[String] = ClassFunctionList()) extends LoggerDisplayProcessor

object LoggerDisplayProcessor {
  implicit val ldp: LoggerDisplayProcessor = SimpleLoggerDisplayProcessor()
  def apply(cf: ClassFunction[_, String]*) = new SimpleLoggerDisplayProcessor(ClassFunctionList(cf.toList))
}