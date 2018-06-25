#==============================================================================#
#                                   KNStudio                                   #
#==============================================================================#
#' KNStudio
#'
#' \code{KNStudio} Builds Kneser Ney language model object.
#'
#' @param x Corpus object upon which the model will be trained.
#' @param modelSize Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#' @param name Character string containing the name of the KN Model object.
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
    #                               Constructor                               #
    #-------------------------------------------------------------------------#
    initialize = function(x, modelSize = 3, open = TRUE, name = NULL)  {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('CVSet')
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
      private$..model <- KN$new(x = x, modelSize = modelSize,
                                open = open , name = name)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Features Method                             #
    #-------------------------------------------------------------------------#
    getFeatures = function() {

    },

    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(corpora, modelSize = 3, open = TRUE, name = NULL) {

      private$..model <- KNBuild$new(private$..model)$build()$getModel()

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Estimate Method                               #
    #-------------------------------------------------------------------------#
    estimate = function() {
      private$..model <- KNEstimate$new(private$..model)$estimate()$getModel()
    },

    #-------------------------------------------------------------------------#
    #                         Evaluation Method                               #
    #-------------------------------------------------------------------------#
    evaluate = function() {
      private$..model <- KNEvaluate$new(private$..model)$evaluate()$getModel()
    },

    #-------------------------------------------------------------------------#
    #                          Get Model Method                               #
    #-------------------------------------------------------------------------#
    getModel = function() private$..model,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knStudio(self)
    }

  )
)
