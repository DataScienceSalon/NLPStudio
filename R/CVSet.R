#' CVSet
#'
#' \code{CVSet} Class representing a cross-validation set
#'
#' Class representing cross-validation sets. A cross-validation contains
#' a training, test, and optional validation set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CVSet class.}
#'   \item{\code{getCorpus(what = NULL)}}{Obtains one or several Corpus objects
#'   from the CVSet. What may contain c("training', 'validation', 'test'). If
#'   NULL all Corpus objects are returned.}
#'   \item{\code{addCorpus(corpus)}}{Adds a Corpus object to the CVSet object.}
#'   \item{\code{removeCorpus(corpus)}}{Removes a Corpus object from the CVSet object.}
#'  }
#'
#' @param x A Corpus object.
#' @param name Character string containing the name for the CVSet object.
#' @param what Character indicating which Corpus object to obtain. Valid values are
#' c("training', 'validation', 'test', NULL).
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVSet <- R6::R6Class(
  classname = "CVSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadServices()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Corpora Management                            #
    #-------------------------------------------------------------------------#
    getCorpus = function(what = NULL) {

      if (length(what) == 0) {
        result <- private$..children
      } else {
        if (grepl("^train", what, ignore.case = TRUE)) {
          what <- "Training"
        } else if (grepl("^val", what, ignore.case = TRUE)) {
          what <- "Validation"
        } else if (grepl("^test", what, ignore.case = TRUE)) {
          what <- "Test"
        }

        key <- 'cv'
        value <- paste(what, "Set")
        listCondition <- private$search(key, value)
        result <- private$..children[listCondition]
      }

      if (is.null(result)) {
        event <- paste0("No ", what, " corpus in the cross-validation set. ",
                        "See ?", class(self)[1], " for further assistance.")
        private$logR$log(method = 'getCorpus', event = event, level = "Warn")
      }

      if (length(result) == 1) result <- result[[1]]

      return(result)
    },

    addCorpus = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addCorpus',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    removeCorpus = function(x) {
      private$detach(x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvSet(self)
    }
  )
)
