#' CVSet
#'
#' \code{CVSet} Class containing training, test and an optional validation corpora objects.
#'
#' Class conitains a cross-validation set of corpora objects. CVSet objects are
#' collections including a training, test, and an optional validation Corpus objects.
#'
#' @section Core Methods:
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
#' @family CVSet Classes
#' @export
CVSet <- R6::R6Class(
  classname = "CVSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite0,

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadServices(name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Corpus Management Methods                         #
    #-------------------------------------------------------------------------#
    addCorpus = function(x) {

      # Validate
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addCorpus', event = v$msg, level = "Error")
        stop()
      }

      private$attach(x)

      event <- paste0("Added ", x$getName(), " to CVSet.")
      private$logR$log(method = 'addCorpus', event = event, level = "Info")
      invisible(self)
    },

    removeCorpus = function(x) {
      private$detach(x)
      event <- paste0("Removed ", x$getName(), " from CVSet.")
      private$logR$log(method = 'removeCorpus', event = event, level = "Info")
      invisible(self)
    },

    purgeCorpora = function() {
      private$..children <- list()
      private$..inventory <- data.frame()
      event <- paste0("Purged all training, validation and test corpora from CVSet.")
      private$logR$log(method = 'purgeSets', event = event, level = "Info")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Get Methods                               #
    #-------------------------------------------------------------------------#
    getTrainingSet = function() {
      train <- private$..children[private$search(key = 'cv', value = 'training')]
      return(train[[1]])
    },

    getValidationSet = function() {
      validation <- private$..children[private$search(key = 'cv', value = 'validation')]
      if (length(validation) == 0) {
        event <- "No validation set exists for the CVSet."
        private$logR$log(method = 'getValidationSet', event = event, level = "Info")
        return(FALSE)
      } else {
        return(validation[[1]])
      }
    },

    getTestSet = function() {
      test <- private$..children[private$search(key = 'cv', value = 'test')]
      return(test[[1]])
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvSet(self)
    }
  )
)
