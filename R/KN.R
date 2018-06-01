#==============================================================================#
#                                   KN                                         #
#==============================================================================#
#' KN
#'
#' \code{KN} Kneser Ney language model object.
#'
#' Kneser Ney language model object.
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
#' @export
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..model = character()
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(corpora, modelSize = 3, open = TRUE, name = NULL)  {

      private$loadDependencies()

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
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Create Language Model Object
      private$..model <- KNModel$new(corpora = corpora, modelSize = modelSize,
                                     open = open , name = name)

      # Create log entry
      event <- paste0("KN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    build = function() {

      private$..model <- KNCounts$new(private$..model)$build()
      private$..model <- KNEstimate$new(private$..model)$build()

      return(private$..model)
    },

    summary = function() {
      private$overview()
      private$..document$summary(section = c("i", "q"))
      private$discountSummary()
      private$nGramSummary()
      private$nGramDetail()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }

  )
)
