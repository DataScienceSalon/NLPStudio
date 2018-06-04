#==============================================================================#
#                               Director                                       #
#==============================================================================#
#' Director
#'
#' \code{Director} Class responsible for executing an NLP Pipeline
#'
#' @section Director methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a Director.}
#'   \item{\code{addCommand()}}{Method that adds a text processing command to the queue. }
#'   \item{\code{removeCommand()}}{Method that removes a command from the queue.}
#'   \item{\code{execute()}}{Method that executes the job queue. }
#'   \item{\code{getResult()}}{Method that returns the object following execution of the job queue. }
#'  }
#'
#' @param cmd The class encapsulating a particular command.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Director classes
#' @export
Director <- R6::R6Class(
  classname = "Director",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..jobLog = data.frame()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadServices()
      event <- paste0("Director object instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Command Management                            #
    #-------------------------------------------------------------------------#
    add = function(cmd) {

      if (!c("App0") %in% class(cmd)) {
        event <- paste0("Invalid Application object. Object must be derived ",
                                  "from the App0 class.  See ?", class(self)[1],
                                  " for further assistance.")
        private$logR$log(method = 'add', event = event, level = "Error")
        stop()
      }

      id <- cmd$getId()
      private$..jobQueue[[id]] <- cmd

      event <- paste0("Added ", class(cmd)[1], " (", id, ") to job queue." )
      private$logR$log(method = 'add', event = event)

      invisible(self)
    },

    remove = function(cmd) {

      id <- cmd$getMeta(key = 'id')
      private$..jobQueue[[id]] <- NULL

      event <- paste0("Removed ", class(cmd)[1], " (", id, ") from job queue." )
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

        event <- paste0("Director pipeline complete." )
        private$logR$log(method = 'execute', event = event)

        invisible(private$..x)
      } else {
        event <- paste0("Director job queue is empty.")
        private$logR$log(method = 'execute', event = event, level = 'Warn')
      }
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$Director(self)
    }
  )
)
