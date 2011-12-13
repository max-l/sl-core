package net.strong_links.core

// Extended logger; wrapper around basic logger, which allows the use of Logging Parameters as arguments.

class Xlogger(logger: Logger) {

  var enableDebug = false

  def debug(params: LoggingParameter*) = if (enableDebug) logger.debug(LoggingParameter.format(params: _*))

  def info(params: LoggingParameter*) = logger.info(LoggingParameter.format(params: _*))

  def warning(params: LoggingParameter*) = logger.warning(LoggingParameter.format(params: _*))

  def error(params: LoggingParameter*) = logger.error(LoggingParameter.safeFormat(params: _*))
}
