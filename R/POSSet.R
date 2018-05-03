#' POSSet
#'
#' \code{POSSet} Class containing a collection of POS objects
#'
#' Class contains POS objects
#'
#' @usage pos <- POSSet$new(x)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the POSSet class.}
#'   \item{\code{tag()}}{Creates POS tags}
#'   \item{\code{get()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the POSSet object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the POSSets
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return POSSet object, containing the POSSet text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family POS Classes
#' @export
POSSet <- R6::R6Class(
  classname = "POSSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

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

    #-------------------------------------------------------------------------#
    #                           Get Methods                                   #
    #-------------------------------------------------------------------------#
    get = function() { private$..documents },
    getCorpus = function() { private$..x },

    #-------------------------------------------------------------------------#
    #                          Composite Management                           #
    #-------------------------------------------------------------------------#
    addTags = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('POS'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addTags',
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
      visitor$posSet(self)
    }
  )
)
