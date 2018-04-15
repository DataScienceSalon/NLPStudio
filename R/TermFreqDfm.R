#' TermFreqDfm
#'
#' \code{TermFreqDfm} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqFactoryDfm}} class.
#'
#' TermFreqDfm objects are wrappers for the \code{\link[quanteda]{dfm}} class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqDfm class.}
#'   \item{\code{dfm(value)}}{Active binding that sets the value of the dfm object via
#'   assignment.  If no value is assigned, the method returns the current quanteda dfm object.}
#'   }
#'
#' @param x A quanteda dfm object.
#' @template metadataParams
#'
#' @return The dfm method returns a quanteda dfm object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqDfm <- R6::R6Class(
  classname = "TermFreqDfm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TermFreq0,

  private = list(
    ..dfm = character()
  ),

  active = list(
    dfm = function(value) {
      if (missing(value)) {
        private$..dfm
      } else {
        if (quanteda::is.dfm(value)) {
          private$..dfm <- value
        } else {
          event <- "Parameter is not a valid dfm object."
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
    initialize = function(name = NULL) {

      private$loadDependencies()

      private$coreMeta(name = name,
                       type = "quanteda dfm")
      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqDfm(self)
    }
  )
)
