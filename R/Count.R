#' Count
#'
#' \code{Count} Class representing tokenized Document objects.
#'
#' Class containing the tokenized representation of a Document object.
#'
#' @usage tokens <- Count$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the Count class.}
#'   \item{\code{content}}{Active binding used to set and retrieve Count content. Count
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   Count content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return Count object, containing the tokens for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Count Classes
#' @export
Count <- R6::R6Class(
  classname = "Count",
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
    initialize = function(x, type) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Corpus')
      private$..params$discrete$variables = list('type')
      private$..params$discrete$values = list(type)
      private$..params$discrete$valid = list(c('tdm', 'dfm', 'dtm'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$meta <- Meta$new(x = self)
      private$meta$set(key = 'type', value = type, type = 'f')
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    get = function() { return(private$..content) },
    getType = function() { return(private$meta$get(key = 'type', type = 'f')) },
    getDocument = function() { return(private$..x) },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$count(self)
    }
  )
)
