#------------------------------------------------------------------------------#
#                     Term Frequency Matrix Interface                          #
#------------------------------------------------------------------------------#
#' TermFreqFactory0
#'
#' \code{TermFreqFactory0}  Abstract "strategy" class that defines interface for the TermFreq classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @family Term Frequency Classes
#' @export
TermFreqFactory0 <- R6::R6Class(
  classname = "TermFreqFactory0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(
    ..x = character(),
    ..settings = list(
      tolower = logical(),
      stem = logical(),
      dictionary = character()
    ),
    ..termFreq = character()
  ),
  public = list(

    initialize = function(x, tolower = TRUE, stem = FALSE, dictionary = NULL) {
      stop("This method is not implemented for this abstract class.")
    }
  )
)
