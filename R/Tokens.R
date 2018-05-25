#' Tokens
#'
#' \code{Tokens} Class representing tokenized Document objects.
#'
#' Class containing the tokenized representation of a Document object.
#'
#' @usage tokens <- Tokens$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the Tokens class.}
#'   \item{\code{content}}{Active binding used to set and retrieve Tokens content. Tokens
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   Tokens content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return Tokens object, containing the tokens for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Tokens Classes
#' @export
Tokens <- R6::R6Class(
  classname = "Tokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(

    ..tokenType = character(),

    setQuant = function(x) {
      private$meta$set(key = private$..tokenType, value = length(x), type = 'q')
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(tokenType) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$classes$name <- list('tokenType')
      private$..params$classes$objects <- list(tokenType)
      private$..params$classes$valid <- list(c('character', 'sentence', 'word'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..tokenType <- tokenType
      private$meta <- Meta$new(x = self)
      private$meta$set(key = 'tokenType', value = tokenType, type = 'f')
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    nTokens = function() {
      return(length(private$..content))
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokens(self)
    }
  )
)
