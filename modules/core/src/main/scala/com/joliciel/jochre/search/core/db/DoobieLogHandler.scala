package com.joliciel.jochre.search.core.db

import doobie.util.log.{ExecFailure, LogEvent, LogHandler, ProcessingFailure, Success}
import org.slf4j.Logger
import org.slf4j.event.Level
import zio.{Task, ZIO}

object DoobieLogHandler {
  def logHandler(log: Logger, logLevel: Level = Level.DEBUG): LogHandler[Task] = (logEvent: LogEvent) => ZIO.attempt {
    logEvent match {
      case Success(s, a, _, e1, e2) =>
        val message =
          s"""Successful Statement Execution:
             |
             |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
             |
             | arguments = [${a.mkString(", ")}]
             |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (${(e1 + e2).toMillis.toString} ms total)
          """.stripMargin

        log.atLevel(logLevel).log(message)

      case ProcessingFailure(s, a, _, e1, e2, t) =>
        log.error(
          s"""Failed Resultset Processing:
             |
             |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
             |
             | arguments = [${a.mkString(", ")}]
             |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (failed) (${(e1 + e2).toMillis.toString} ms total)
             |   failure = ${t.getMessage}
      """.stripMargin)

      case ExecFailure(s, a, _, e1, t) =>
        log.error(
          s"""Failed Statement Execution:
             |
             |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
             |
             | arguments = [${a.mkString(", ")}]
             |   elapsed = ${e1.toMillis.toString} ms exec (failed)
             |   failure = ${t.getMessage}
      """.stripMargin)
    }
  }
}
