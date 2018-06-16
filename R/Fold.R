#' Fold
#'
#' \code{Fold} Class containing 1 of 'K' folds.  Eeach fold containing a training and test set.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the Fold class.}
#'  }
#'
#' @param name Character string containing the name for the Fold object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Set Classes
#' @export
Fold <- R6::R6Class(
  classname = "Fold",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadServices(name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Set Management                           #
    #-------------------------------------------------------------------------#
    getSets = function() { return(private$..children) },

    getTrainingSet = function() {
      train <- private$..children[private$search(key = 'cv', value = 'training')]
      return(train[[1]])
    },

    getTestSet = function() {
      test <- private$..children[private$search(key = 'cv', value = 'test')]
      return(test[[1]])
    },

    addSet = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addSet',
                         event = v$msg, level = "Error")
        stop()
      }

      private$attach(x)
      event <- paste0("Added ", x$getName(), " to Fold.")
      private$logR$log(method = 'addSet', event = event, level = "Info")

      invisible(self)

    },

    removeSet = function(x) {
      private$detach(x)
      invisible(self)
    },

    purgeSets = function() {
      private$..children <- list()
      private$..inventory <- data.frame()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fold(self)
    }
  )
)
