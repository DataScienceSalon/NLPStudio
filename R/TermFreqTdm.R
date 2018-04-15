#' TermFreqTdm
#'
#' \code{TermFreqTdm} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqFactoryTdm}} class.
#'
#' TermFreqTdm objects are wrappers for the tm package TermDocumentMatrix class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqTdm class.}
#'   \item{\code{tdm()}}{Active binding that returns the TermDocumentMatrix object.
#'   object. The current TermDocumentMatrix is set through assignment.}
#'   }
#'
#' @param x A tm package TermDocumentMatrix object.
#' @template metadataParams
#'
#' @return The tdm method returns a TermDocumentMatrix object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqTdm <- R6::R6Class(
  classname = "TermFreqTdm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TermFreq0,

  private = list(
    ..tdm = character()
  ),

  active = list(
    tdm = function(value) {
      if (missing(value)) {
        private$..tdm
      } else {
        if (class(value)[1] %in% c("TermDocumentMatrix")) {
          private$..tdm <- value
        } else {
          event <- "Parameter is not a valid TermDocumentMatrix object."
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
      visitor$termFreqTdm(self)
    }
  )
)
