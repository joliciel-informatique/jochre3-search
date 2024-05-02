package com.joliciel.jochre.search.api.utilities

import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.api.{HttpError, HttpErrorMapper, OkResponse, UnknownLogLevelException}
import org.slf4j.LoggerFactory
import zio.ZIO

trait UtilityLogic extends HttpErrorMapper {

  def putLogLevelLogic(prefix: String, level: String): ZIO[Requirements, HttpError, OkResponse] = ZIO
    .attempt {
      // The Level object always applies a default level if it doesn't understand the String
      // An unknown level is one where the default level is selected, although it wasn't requested
      val newLevel = Level.toLevel(level, Level.ALL)
      if (newLevel.equals(Level.ALL) && level != Level.ALL.levelStr) {
        throw new UnknownLogLevelException(level)
      } else {
        // assume SLF4J is bound to logback in the current environment
        val loggerContext: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
        val logger: Logger = loggerContext.getLogger(prefix)
        logger.setLevel(newLevel)
        logger.trace("Testing trace level")
        logger.debug("Testing debug level")
        logger.info("Testing info level")
        OkResponse()
      }
    }
    .tapErrorCause(error => ZIO.logErrorCause(s"Unable to update log level", error))
    .mapError(mapToHttpError)

  def resetLogLogic(): ZIO[Requirements, HttpError, OkResponse] = ZIO
    .attempt {
      // assume SLF4J is bound to logback in the current environment
      val context: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

      val configurator = new JoranConfigurator
      configurator.setContext(context)
      context.reset()
      OkResponse()
    }
    .tapErrorCause(error => ZIO.logErrorCause(s"Unable to reset log level", error))
    .mapError(mapToHttpError)
}
