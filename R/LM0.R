#==============================================================================#
#                                   LM0                                        #
#==============================================================================#
#' LM0
#'
#' \code{LM0} Abstract class for the language model family of classes.
#'
#' @param x Corpus object upon which the model will be trained.
#' @param size Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @export
LM0 <- R6::R6Class(
  classname = "LM0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..document = character(),
    ..modelType = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),
    ..tables = list()
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(x, size = 3, open = TRUE, name = NULL) {
      stop("This method is not implemented for this abstract class.")
    },
    #-------------------------------------------------------------------------#
    #                           Setters / Getters                             #
    #-------------------------------------------------------------------------#
    getName = function() { return(private$meta$get(key = 'name'))},
    getCorpus = function() { return(private$..x) },
    getType = function() { return(private$meta$get(key = 'modelType')) },
    getSmoothing = function() { return(private$meta$get(key = 'smoothing')) },
    getSize = function() { return(private$meta$get(key = 'modelSize')) },
    is.openVocabulary = function() { return(private$meta$get(key = 'openVocabulary')) },
    getDocument = function() { return(private$..document) },
    setDocument = function(x) {
      private$..document <- x
      invisible(self)
    }
  )
)
