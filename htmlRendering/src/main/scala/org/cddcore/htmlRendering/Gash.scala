package org.cddcore.htmlRendering

import org.cddcore.utilities.Files
import java.io.PrintWriter
import java.io.InputStreamReader
import org.stringtemplate.v4.STGroupFile
import org.stringtemplate.v4.ST

class Gash

object Gash {
  def main(args: Array[String]) {
    def loadString(name: String) = Files.getFromClassPath(classOf[Gash], name)
    def render(s: String, kvs: (String, Any)*) = {
      val st = new ST(s, '$', '$')
      for ((k, v) <- kvs)
        st.add(k, v)
      st.render()
    }
    val reportString = loadString("report.st")
    def report(reportName: String, reportDate: String) = render(reportString, "reportName" -> reportName, "reportDate" -> reportDate)
    println(report("Some Report Name", "Some Report Date"))
  }
}