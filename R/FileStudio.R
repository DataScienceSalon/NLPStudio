#==============================================================================#
#                               FileStudio                                     #
#==============================================================================#
#' FileStudio
#'
#' \code{FileStudio} Class for performing file cleaning and preprocessing
#'
#' @template fileStudioClasses
#'
#' @section FileStudio methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a FileStudio.}
#'   \item{\code{addCommand()}}{Method that adds a file processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' @section Parameters:
#' @param x The object to be processed.
#' @param queue The job queue containing file processing commands.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family FileStudio classes
#' @export
FileStudio <- R6::R6Class(
  classname = "FileStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$classes$name <- list("x")
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c("FileCollection", "File"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      if (class(x)[1] == 'FileCollection') private$..x <- CloneFile$new()$collection(x = x)
      if (class(x)[1] == 'File') private$..x <- CloneFile$new()$file(x = x)

      # Create log entry
      event <- paste0("FileStudio object instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Command Management                            #
    #-------------------------------------------------------------------------#
    add = function(cmd) {

      if (!c("FileStudio0") %in% class(cmd)) {
        event <- paste0("Invalid FileStudio object. Object must be ",
                                  "of the FileStudio0 classes.  See ?FileStudio0",
                                  " for further assistance.")
        private$logR$log(method = 'add', event = event, level = "Error")
        stop()
      }

      name <- class(cmd)[1]
      private$..jobQueue[[name]] <- cmd

      event <- paste0("Added ", name, " to ", private$..x$getName(),
                                " job queue." )
      private$logR$log(method = 'add', event = event)

      invisible(self)
    },

    remove = function(cmd) {

      name <- class(cmd)[1]
      private$..jobQueue[[name]] <- NULL

      event <- paste0("Removed ", name, " from ", private$..x$getName(),
                                " job queue." )
      private$logR$log(method = 'remove', event = event)
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                       Execute and Return Results                        #
    #-------------------------------------------------------------------------#
    execute = function() {

      if (length(private$..jobQueue) > 0) {

        for (i in 1:length(private$..jobQueue)) {
          private$..x <- private$..jobQueue[[i]]$execute(private$..x)
        }

        event <- paste0("Executed FileStudio commands on ",
                                  private$..x$getName(), "." )
        private$logR$log(method = 'execute', event = event)

        invisible(private$..x)
      } else {
        event <- paste0("FileStudio job queue is empty.")
        private$logR$log(method = 'execute', event = event, level = 'Error')
        stop()
      }

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
