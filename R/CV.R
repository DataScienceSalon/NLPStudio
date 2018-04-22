#' CV
#'
#' \code{CV} Class containing cross-validation or CVSet objects.
#'
#' Class containing cross-validation or CVSet objects.
#'
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CV class.}
#'   \item{\code{summary()}}{Summarizes the CV object.}
#'  }
#'
#' @param name Character string containing the name for the CV object.
#' @template metadataParams
#'
#' @return CV object, containing the CVSets.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CV <- R6::R6Class(
  classname = "CV",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Document Management                           #
    #-------------------------------------------------------------------------#
    getCVSets = function() { private$..documents },

    addCVSet = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('CVSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addCVSet',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    removeCVSet = function(x) {
      private$detach(x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cv(self)
    }
  )
)
