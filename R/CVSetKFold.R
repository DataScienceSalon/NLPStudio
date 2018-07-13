#' CVSetKFold
#'
#' \code{CVSetKFold} Class containing k CVSets created by the KFold Class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CVSetKFold class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the CVSetKFold object.}
#'  }
#'
#' @param name Character string containing the name for the CVSetKFold object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'traian', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the CVSetKFolds
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return CVSetKFold object, containing the CVSetKFold text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CVSet Classes
#' @export
CVSetKFold <- R6::R6Class(
  classname = "CVSetKFold",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadServices(name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Composite Management                              #
    #-------------------------------------------------------------------------#
    getCVSets = function(k = NULL) {
      if (is.null(k)) {
        return(private$..children)
      } else {
        return(private$..children[[k]])
      }
    },

    addCVSet = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('CVSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addCVSet',
                         event = v$msg, level = "Error")
        stop()
      }

      private$attach(x)
      event <- paste0("Added CVSet, ", x$getName(), " to CVSetKFold Object.")
      private$logR$log(method = 'addCVSet', event = event, level = "Info")

      invisible(self)

    },

    removeCVSet = function(x) {
      private$detach(x)
      event <- paste0("Removed CVSet, ", x$getName(), " from CVSetKFold Object.")
      private$logR$log(method = 'removeCVSet', event = event, level = "Info")
      invisible(self)
    },

    purgeCVSets = function() {
      private$..children <- list()
      private$..inventory <- data.frame()
      event <- paste0("Purged all CVSets from the CVSetKFold object.")
      private$logR$log(method = 'purgeCVSets', event = event, level = "Info")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$KFolds(self)
    }
  )
)
