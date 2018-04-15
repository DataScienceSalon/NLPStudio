#' TermFreqDtm
#'
#' \code{TermFreqDtm} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqFactoryDtm}} class.
#'
#' TermFreqDtm objects are wrappers for the tm package DocumentTermMatrix class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqDtm class.}
#'   \item{\code{tdm()}}{Active binding that returns the DocumentTermMatrix object.
#'   object. The current DocumentTermMatrix is set through assignment.}
#'   }
#'
#' @param x A tm package DocumentTermMatrix object.
#' @template metadataParams
#'
#' @return The tdm method returns a DocumentTermMatrix object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqDtm <- R6::R6Class(
  classname = "TermFreqDtm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TermFreq0,

  private = list(
    ..tdm = character()
  ),

  active = list(
    dtm = function(value) {
      if (missing(value)) {
        private$..dtm
      } else {
        if (class(value)[1] %in% c("DocumentTermMatrix")) {
          private$..dtm <- value
        } else {
          event <- "Parameter is not a valid DocumentTermMatrix object."
          private$logR$log(cls = class(self)[1], event = event, "Error")
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL, corpusId = NULL) {

      private$loadDependencies()

      private$coreMeta(name = name,
                       type = "TermDocumentMatrix",
                       corpusId = corpusId)
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqDtm(self)
    }
  )
)
