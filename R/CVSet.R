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
  inherit = Corpus,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Text Method                                 #
    #-------------------------------------------------------------------------#
    text = function(x = NULL, note = NULL) {
      docs <- self$getDocuments()
      if (is.null(x)) {
        return(as.character(lapply(docs, function(d) { d$text() })))
      } else {
        if (length(docs) == length(x)) {
          if (length(note) == 1) note <- rep(note, length(docs))
          for (i in 1:length(docs)) {
            docs[[i]]$text(x = x[i], note = note[i])
          }
        } else {
          event <- paste0("The 'x' parameter must be a list of texts with ",
                          "length equal to the number of Text documents ",
                          "in the CVSet. See?", class(self)[1],
                          " for further assistance.")
          private$logR$log(method = 'text', event = event, level = "Error")
          stop()
        }
      }
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvSet(self)
    }
  )
)
