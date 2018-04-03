#==============================================================================#
#                                 LogR                                         #
#==============================================================================#
#' LogR
#'
#' \code{LogR} Writes to log
#'
#' Writes to log
#'
#' @section Class methods:
#'
#' \strong{LogR Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Initiates thelogobject. }
#'   \item{\code{writeLog()}}{Writes log.}
#'   \item{\code{queryLog(...)}}{Enables client to perform queries on the log.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
LogR <- R6::R6Class(
  classname = "LogR",
  lock_objects = FALSE,
  lock_class = TRUE,

  private = list(
    ..log = data.frame(),
    ..logPath = './NLPStudio/logs',
    notifyInfo  = function(note) futile.logger::flog.info(note, name = "green"),
    notifyWarn  = function(note) futile.logger::flog.warn(note, name = "yellow"),
    notifyError = function(note)  futile.logger::flog.error(note, name = "red")
  ),

  public = list(
    entry = list(
      className = character(),
      methodName = character(),
      level = character(),
      msg = character(),
      fieldName = character(),
      created = character()
    ),

    initialize = function(logPath = NULL) {

      if (is.null(logPath)) {
        logPath <- private$..logPath
      }

      dir.create(logPath, showWarnings = FALSE, recursive = TRUE)
      futile.logger::flog.threshold(INFO)
      futile.logger::flog.logger("green", INFO, appender=appender.file(file.path(logPath, "green.log")))
      futile.logger::flog.logger("yellow", WARN, appender=appender.tee(file.path(logPath, "yellow.log")))
      futile.logger::flog.logger("red", ERROR, appender=appender.tee(file.path(logPath, "red.log")))

      invisible(self)
    },

    log  = function(cls, event, level = "Info",
                    fieldName = NULL, method = NULL) {

      level <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                    level, perl = TRUE)

      note <- paste0(level, " in class '", cls, "'",
                     ifelse(is.null(method)," ",
                     paste0("method, '", method, "' ")), ifelse(is.null(fieldName), "",
                                           paste0("with variable '",
                                          fieldName, "'. ")), event)
      #Write to log
      switch(level,
             Info  = private$notifyInfo(note),
             Warn  = private$notifyWarn(note),
             Error = private$notifyError(note)
      )

      # Append information log to log for object.
      if (level == "Info") {
        log <- data.frame(user = Sys.info()[['user']],
                          date = Sys.time(),
                          class = cls,
                          event = event)
        private$..log <- rbind(private$..log, log)
      }
    },

    getLog = function() { private$..log }
  )
)
