#' CVSet
#'
#' \code{CVSet} Class representing a cross-validation set.
#'
#' Class representing cross-validation sets. A cross-validation set may be
#' a training, validation, test, or k-fold set of Document objects.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CVSet class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the CVSet object.}
#'  }
#'
#' @param name Character string containing the name for the CVSet object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the CVSets
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return CVSet object, containing the CVSet text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVSet <- R6::R6Class(
  classname = "CVSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Corpora Management                            #
    #-------------------------------------------------------------------------#
    getCorpus = function(what = NULL) {

      # Validate key/value pair
      if (!is.null(what)) {
        private$..params <- list()
        private$..params$discrete$variables <- c('cv')
        private$..params$discrete$values <- list(what)
        private$..params$discrete$valid <- list(c('train', 'validation', 'test'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log( method = 'get',
                            event = v$msg, level = "Error")
          stop()
        }
      }

      # Search for documents that match the metadata and return
      if (!is.null(what)) {
        key <- 'cv'
        value <- what
        listCondition <- private$search(key, value)
        result <- private$..documents[listCondition]
      } else {
        result <- private$..documents
      }

      return(result)
    },

    addCorpus = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addCorpus',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    removeCorpus = function(x) {
      private$detach(x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvSet(self)
    }
  )
)
