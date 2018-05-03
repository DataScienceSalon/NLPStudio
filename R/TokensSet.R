#' TokensSet
#'
#' \code{TokensSet} Class containing a collection of Tokens objects
#'
#' Class contains Tokens objects
#'
#' @usage skiReport <- TokensSet$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the TokensSet class.}
#'   \item{\code{word()}}{Creates word tokens.}
#'   \item{\code{sentence()}}{Creates sentence tokens.}
#'   \item{\code{char()}}{Creates character tokens.}
#'   \item{\code{get()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the TokensSet object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the TokensSets
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return TokensSet object, containing the TokensSet text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Tokens Classes
#' @export
TokensSet <- R6::R6Class(
  classname = "TokensSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  private = list(
    ..x = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    get = function() { private$..documents },
    getCorpus = function() { private$..x },
    nTokens = function() {
      tokenType <- private$meta$get(key = 'type', type = 'f')
      private$meta$get(key = paste0(tokenType, 's'), type = 'q')
    },

    #-------------------------------------------------------------------------#
    #                          Composite Management                           #
    #-------------------------------------------------------------------------#
    addTokens = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Tokens'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addTokens',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokensSet(self)
    }
  )
)
