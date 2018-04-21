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
    ..x = character(),
    ..log = data.frame(),
    ..logPath = './NLPStudio/logs',
    notifyInfo  = function(note) futile.logger::flog.info(note, name = "green"),
    notifyWarn  = function(note) futile.logger::flog.warn(note, name = "yellow"),
    notifyError = function(note)  futile.logger::flog.error(note, name = "red")
  ),

  public = list(

    initialize = function(x, logPath = NULL) {

      private$..x <- x

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

    log  = function(x = NULL, event, level = "Info",
                    fieldName = NULL, method = NULL) {

      level <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                    level, perl = TRUE)

      note <- paste0(level, " in class '", class(private$..x)[1], "', ",
                     ifelse(is.null(method)," ",
                     paste0("method, '", method, "', ")), ifelse(is.null(fieldName), "",
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
        log <- data.frame(class = ifelse(is.null(x),
                                         class(private$..x)[1],
                                         class(x)[1]),
                          event = event,
                          user = Sys.info()[['user']],
                          datetime = Sys.time(),
                          stringsAsFactors = FALSE,
                          row.names = NULL)
        private$..log <- rbind(private$..log, log)
      }
    },

    printLog = function() {
      identity <- private$..x$getMeta(type = 'identity')
      name <- private$..x$getName()
      NLPStudio::printHeading(text = paste0(identity$classname, ": ",
        private$..x$getName(), " Log"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nObject Class : ", identity$classname))
      cat(paste0("\nObject Id    : ", identity$id))
      cat(paste0("\nObject Name  : ", name))
      log <- private$..log

      if (nrow(log) > 0) {
        cat("\n")
        print(log, row.names = FALSE)
        cat("\n")
        return(log)
      } else {
        cat("\n\nThere are no logged events for this object.\n\n")
        return(FALSE)
      }
    }
  )
)
