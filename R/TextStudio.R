#==============================================================================#
#                               TextStudio                                     #
#==============================================================================#
#' TextStudio
#'
#' \code{TextStudio} Class for performing text cleaning and preprocessing
#'
#' @template textStudioClasses
#'
#' @section TextStudio methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a TextStudio.}
#'   \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' @section Parameters:
#' @param object The object to be processed.
#' @param queue The job queue containing text processing commands.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextStudio classes
#' @export
TextStudio <- R6::R6Class(
  classname = "TextStudio",
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
      private$..params$classes$valid <- list(c("Corpus", "Document"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- Clone$new()$this(x = x)

      # Create log entry
      event <- paste0("TextStudio object instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Command Management                            #
    #-------------------------------------------------------------------------#
    add = function(cmd) {

      if (!c("TextStudio0") %in% class(cmd)) {
        event <- paste0("Invalid TextStudio object. Object must be ",
                                  "of the TextStudio0 classes.  See ?TextStudio0",
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

        event <- paste0("Executed TextStudio commands on ",
                                  private$..x$getName(), "." )
        private$logR$log(method = 'execute', event = event)

        invisible(private$..x)
      } else {
        event <- paste0("TextStudio job queue is empty.")
        private$logR$log(method = 'execute', event = event, level = 'Error')
        stop()
      }

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio(self)
    }
  )
)
