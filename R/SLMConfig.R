#==============================================================================#
#                                   SLMConfig                                  #
#==============================================================================#
#' SLMConfig
#'
#' \code{SLMConfig} Class representing a Statistical Language Model (SLM) configuration.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @export
SLMConfig <- R6::R6Class(
  classname = "SLMConfig",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..settings = list(
      modelName = character(),
      modelSize = numeric(),
      openVocabulary = logical(),
      smoothing = character(),
      modelType = character(),
      modelTypes = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram')
    )
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function(smoothing, modelSize = 3, name = NULL,
                          openVocabulary = TRUE) {

      private$loadServices(name)

      # Validation
      private$..params$discrete$variables = list('smoothing')
      private$..params$discrete$values = list(smoothing)
      private$..params$discrete$valid = list(c('katz', 'kn', 'mkn', 'sbo'))
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      private$..params$logicals$variables <- c('openVocabulary')
      private$..params$logicals$values <- c(openVocabulary)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..settings$modelName <- name
      private$..settings$smoothing <- smoothing
      private$..settings$modelSize <- modelSize
      private$..settings$modelType <- private$..settings$modelTypes[modelSize]

      invisible(self)
    },

    getConfig = function() private$..settings,
    getSmoothing = function() private$..settings$smoothing,
    getModelName = function() private$..settings$modelName,
    getModelSize = function() private$..settings$modelSize,
    getModelTypes = function() private$..settings$modelTypes,
    isOpen = function() private$..settings$openVocabulary,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$slmConfig(self)
    }

  )
)
