#==============================================================================#
#                                    MKN                                       #
#==============================================================================#
#' MKN
#'
#' \code{MKN} Class containing the Kneser Ney Language Model object.
#'
#' @param x CVSet object containing a training and test set.
#' @param modelSize Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#' @param name Character string. Represents name to be assigned to the MKN object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @family Kneser-Ney Language Model Classes
#' @export
MKN <- R6::R6Class(
  classname = "MKN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KN0,

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(config, model) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('config', 'model')
      private$..params$classes$objects <- list(config, model)
      private$..params$classes$valid <- list('SLMConfig', 'list')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Initalize member variables
      private$..config <- config
      private$..model <- model

      # Unpack configuration and initialize meta data
      cfg <- config$getConfig()
      private$meta$set(key = 'smoothing', value = "Modified Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = cfg$modelSize, type = 'f')
      private$meta$set(key = 'modelType', value = cfg$modelTypes[cfg$modelSize], type = 'f')
      private$meta$set(key = 'openVocabulary', value = cfg$openVocabulary, type = 'f')

      # Create log entry
      event <- paste0("MKN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mkn(self)
    }
  )
)
