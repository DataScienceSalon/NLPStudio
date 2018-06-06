#' CVKFold
#'
#' \code{CVKFold} Class representing a K-FOLD collection of cross-validation sets
#'
#' This composite class contains a collection of K CVSet objects.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CVKFold class.}
#'   \item{\code{getCVSet(what = NULL)}}{Obtains one or several CVSet objects
#'   from the CVKFold.}
#'   \item{\code{addCVSet(corpus)}}{Adds a CVSet object to the CVKFold object.}
#'   \item{\code{removeCVSet(corpus)}}{Removes a CVSet object from the CVKFold object.}
#'  }
#'
#' @param x A CVSet object.
#' @param name Character string containing the name for the CVKFold object.
#' @param k Numeric index into the enclosed CVSets, used for retrieving
#' a CVSet by index.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVKFold <- R6::R6Class(
  classname = "CVKFold",
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
    #                            CVSet Management                             #
    #-------------------------------------------------------------------------#
    getCVSet = function(k = NULL) {

      if (!is.null(k)) {
        # Validate class of object.
        private$..params <- list()
        private$..params$classes$name <- list('k')
        private$..params$classes$objects <- list(k)
        private$..params$classes$valid <- list(c('numeric', 'integer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'getCVSet',
                           event = v$msg, level = "Error")
          stop()
        }
        if (length(private$..children) < k) {
          event <- paste("Invalid 'k' parameter. There are only",
                         length(private$..children), "CVSet objects in the",
                         "CVKfold Object.")
          private$logR$log(method = 'getCVSet', event = event, level = "Error")
          stop()
        } else {
          cvSets <- private$..children[[k]]
        }
      } else {
        cvSets <- private$..children
      }

      if (length(cvSets) == 1) cvSets <- cvSets[[1]]

      return(cvSets)
    },

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
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvKFold(self)
    }
  )
)
