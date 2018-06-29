#==============================================================================#
#                                    KN                                        #
#==============================================================================#
#' KN
#'
#' \code{KN} Class containing the Kneser Ney Language Model object.
#'
#' @param x CVSet object containing a training and test set.
#' @param modelSize Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#' @param name Character string. Represents name to be assigned to the KN object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @family Kneser-Ney Language Model Classes
#' @export
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(config, model) {

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
      private$meta$set(key = 'smoothing', value = "Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = cfg$modelSize, type = 'f')
      private$meta$set(key = 'modelType', value = cfg$modelTypes[cfg$modelSize], type = 'f')
      private$meta$set(key = 'openVocabulary', value = cfg$openVocabulary, type = 'f')

      # Create log entry
      event <- paste0("KN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    getNGrams = function() private$..model$nGrams,
    setNGrams = function(x) private$..model$nGrams <- x,
    getDiscounts = function() private$..model$discounts,
    getTotals = function() private$..model$totals,
    getScores = function() private$..model$scores,
    setScores = function(x) private$..model$scores <- x,
    getEval = function() private$..model$evaluation,
    setEval = function(x) private$..model$evaluation <- x,

    summary = function() {
      train <- private$..corpora$getTrain()
      test <- private$..corpora$getTest()
      private$overview()
      train$summary(section = c("i", "q"))
      test$summary(section = c("i", "q"))
      private$discountSummary()
      private$nGramSummary()
      private$nGramDetail()
      private$evaluation()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }
  )
)
