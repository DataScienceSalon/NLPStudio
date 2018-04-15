#' Tokens
#'
#' \code{Tokens} Class representing a Tokenized Corpus.
#'
#' Tokens objects are Corpus level objects containing collections of
#' Tokenized Document objects.
#'
#' @usage tokenizedCorpus <- Tokens$new(name = 'Sentence Tokens', what = "sentence")
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Tokens class.}
#'  }
#'
#' @param x The Corpus object to be tokenized.
#' @param name Character string containing the name to assign to the Tokenized Corpus
#' @param what Character string indicating the level of tokenization. Valid
#' values are c("sentence", "word", "character").
#' @template metadataParams
#'
#' @return Tokens object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data Classes
#' @family Tokens Classes
#' @export
Tokens <- R6::R6Class(
  classname = "Tokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..documentTokens = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Constructor Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()
      private$meta <- Meta$new(x = self)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Tokens Management Methods                         #
    #-------------------------------------------------------------------------#
    get = function() { private$..documentTokens },
    add = function(x) {
      name <- names(x)
      private$..documentTokens <- c(private$..documentTokens, list(x))
    },




    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokens(self)
    }
  )
)
