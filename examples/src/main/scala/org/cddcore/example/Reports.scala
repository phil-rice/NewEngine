package org.cddcore.example

import org.cddcore.engine.Project
import org.cddcore.example.customerCategorisation.CategorisePerson
import org.cddcore.example.processCheque_DM_Xml.ProcessChequeXml
import org.cddcore.example.tennisScore.TennisScorer
import org.cddcore.htmlRendering.ReportOrchestrator

object Reports {

  def main(args: Array[String]) {
    new ReportOrchestrator("C:\\cdd", "Example", List(CategorisePerson.categorise, ProcessChequeXml.processCheque, TennisScorer.scorer)).makeReports;
    println("done")
  }
}