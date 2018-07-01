#==============================================================================#
#                               KNBuild                                        #
#==============================================================================#
#' KNBuild
#'
#' \code{KNBuild} Computes counts used to estimate Kneser Ney probabilities
#'
#' Computes counts used in the probability calculations for the Kneser Ney
#' probabilities.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family SLMStudio Classes
#' @export
KNBuild <- R6::R6Class(
  classname = "KNBuild",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNBuild0,

  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(config, corpora) {

      private$loadServices()

      private$..params <- list()
      private$..params$classes$name <- list('config', 'corpora')
      private$..params$classes$objects <- list(config, corpora)
      private$..params$classes$valid <- list('SLMConfig','SLMCorpora')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..config <- config
      private$..corpora <- corpora

      event <-  paste0("Instantiated KNBuild ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    getModel = function() {
      model <- KN$new(config = private$..config, model = private$..model)
      return(model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knBuild(self)
    }
  )
)
