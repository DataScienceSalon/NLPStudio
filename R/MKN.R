#==============================================================================#
#                                   MKN                                        #
#==============================================================================#
#' MKN
#'
#' \code{MKN} Modified Kneser Ney language model object.
#'
#' Modified Kneser Ney language model object.
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
#' @family LMStudio Classes
#' @export
MKN <- R6::R6Class(
  classname = "MKN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = LM0,

  private = list(
    ..x = character(),
    ..size = numeric(),
    ..open = logical()
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(x, size = 3, open = TRUE, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
        name <- paste0(name, " Modified Kneser-Ney Model")
      }

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      private$..params$logicals$variables <- list("open")
      private$..params$logicals$values <- list(open)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      } else if (size < 1 | size > 5) {
        event <- "Model size must be between 1 and 5."
        private$logR$log(method = 'initialize',
                         event = event, level = "Error")
        stop()
      }

      # Initialize private members
      private$..x <- x
      private$meta$set(key = 'smoothing', value = "Modified Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = size, type = 'f')
      private$meta$set(key = 'modelType', value = private$..modelType[size], type = 'f')
      private$meta$set(key = 'openVocabulary', value = open, type = 'f')

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
