package com.github.pedrovgs.kuronometer.free.interpreter

import cats.{Id, ~>}
import com.github.pedrovgs.kuronometer.free.algebra.{
  GetTodayBuildExecution,
  GetTotalBuildExecution,
  ReportBuildExecution,
  ReporterOp
}
import com.github.pedrovgs.kuronometer.free.domain.{LocalReport, RemoteReport}
import com.github.pedrovgs.kuronometer.free.interpreter.api.KuronometerApiClient
import com.github.pedrovgs.kuronometer.free.interpreter.csv.CsvReporter
import com.github.pedrovgs.kuronometer.implicits._

class ReporterInterpreter(implicit csvReporter: CsvReporter,
                          apiClient: KuronometerApiClient,
                          clock: Clock)
    extends (ReporterOp ~> Id) {

  override def apply[A](fa: ReporterOp[A]): Id[A] = fa match {
    case ReportBuildExecution(buildExecution, RemoteReport) =>
      apiClient.report(buildExecution)
    case ReportBuildExecution(buildExecution, LocalReport) =>
      csvReporter.report(buildExecution)
    case GetTotalBuildExecution() =>
      csvReporter.getTotalBuildExecutionStages
    case GetTodayBuildExecution() =>
      csvReporter.getBuildExecutionStagesSinceTimestamp(
        clock.todayMidnightTimestamp)
  }

}


object ReporterInterpreter{
  import com.github.pedrovgs.kuronometer.free.algebra.Reporter
  import com.github.pedrovgs.kuronometer.KuronometerResults.KuronometerResult
  import com.github.pedrovgs.kuronometer.free.domain.BuildExecution
  import com.github.pedrovgs.kuronometer.free.domain.{Report, SummaryBuildStagesExecution}

  def CvsWebReporter(implicit csvReporter: CsvReporter,
                       apiClient: KuronometerApiClient,
                       clock: Clock) = new Reporter[KuronometerResult]{

    def reportBuildExecution(buildExecution: BuildExecution,
        report: Report): KuronometerResult[BuildExecution] =
      report match {
        case RemoteReport =>
          apiClient.report(buildExecution)
        case LocalReport =>
          csvReporter.report(buildExecution)
      }

    def getTotalBuildExecution(): KuronometerResult[SummaryBuildStagesExecution] =
      csvReporter.getTotalBuildExecutionStages

    def getTodayBuildExecution(): KuronometerResult[SummaryBuildStagesExecution] =
      csvReporter.getBuildExecutionStagesSinceTimestamp(clock.todayMidnightTimestamp)
  }
}
