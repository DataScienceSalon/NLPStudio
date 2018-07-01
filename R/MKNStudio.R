#==============================================================================#
#                                   MKNStudio                                   #
#==============================================================================#
#' MKNStudio
#'
#' \code{MKNStudio} Builds Kneser Ney language model object.
#'
#' @param config SLMConfig object containing the model parameters
#' @param corpora SLMCorpora object containing the test set.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @family Kneser Ney Model Classes
#' @export
MKNStudio <- R6::R6Class(
  classname = "MKNStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLMStudio0,

  public = list(
    #-------------------------------------------------------------------------#
    #                               Constructor                               #
    #-------------------------------------------------------------------------#
    initialize = function(config, corpora)  {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('config', 'corpora')
      private$..params$classes$objects <- list(config, corpora)
      private$..params$classes$valid <- list('SLMConfig', 'SLMCorpora')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build',
                         event = v$msg, level = "Error")
        stop()
      }

      # Create Language Model Object
      private$..config <- config
      private$..corpora <- corpora

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..model <- MKNBuild$new(private$..config,
                                     private$..corpora)$build()$getModel()
      private$..model <- MKNEstimate$new(private$..config,
                                        private$..model)$estimate()$getModel()

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                          Get Model Method                               #
    #-------------------------------------------------------------------------#
    getModel = function() private$..model,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknStudio(self)
    }
  )
)
