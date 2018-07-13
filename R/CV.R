#' CV
#'
#' \code{CV} Class containing training, test and an optional validation corpora objects.
#'
#' Class conitains a cross-validation set of corpora objects. CV objects are
#' collections including a training, test, and an optional validation Corpus objects.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CV class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the CV object.}
#'  }
#'
#' @param name Character string containing the name for the CV object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the CVs
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return CV object, containing the CV text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CV Classes
#' @export
CV <- R6::R6Class(
  classname = "CV",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    createCV = function() {
      private$..cvSets <- KFold$new()$execute(x = private$..x,
                                                   k = private$..kFolds,
                                                   stratify = private$..stratify,
                                                   seed = private$..seed)$getKFolds()
    }
  ),

  public = list(


    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cv(self)
    }
  )
)
