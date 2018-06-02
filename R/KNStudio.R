#==============================================================================#
#                                   KNStudio                                   #
#==============================================================================#
#' KNStudio
#'
#' \code{KNStudio} Builds Kneser Ney language model object.
#'
#' @param x Corpus object upon which the model will be trained.
#' @param size Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @family Kneser Ney Model Classes
#' @export
KNStudio <- R6::R6Class(
  classname = "KNStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = LMStudio0,

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function()  {

      private$loadDependencies()
      invisible(self)
    },

    build = function(corpora, modelSize = 3, open = TRUE, name = NULL) {

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpora')
      private$..params$classes$objects <- list(corpora)
      private$..params$classes$valid <- list('LMCorpora')
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      private$..params$logicals$variables <- c('open')
      private$..params$logicals$values <- c(open)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build',
                         event = v$msg, level = "Error")
        stop()
      }

      # Create Language Model Object
      private$..model <- KN$new(corpora = corpora, modelSize = modelSize,
                                open = open , name = name)


      private$..model <- KNCounts$new(private$..model)$build()
      private$..model <- KNEstimate$new(private$..model)$build()
      private$..model <- KNEvaluate$new(private$..model)$build()

      return(private$..model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knStudio(self)
    }

  )
)
