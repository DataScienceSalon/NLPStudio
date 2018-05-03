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
    ..x = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Document')
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

    get = function() { return(private$..content) },
    getDocument = function() { return(private$..x) },
    nTokens = function() {
      tokenType <- private$meta$get(key = 'type', type = 'f')
      private$meta$get(key = paste0(tokenType, 's'), type = 'q')
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokens(self)
    }
  )
)
