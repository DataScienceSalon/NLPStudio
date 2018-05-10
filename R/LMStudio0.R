#==============================================================================#
#                               LMStudio0                                      #
#==============================================================================#
#' LMStudio0
#'
#' \code{LMStudio0} Abstract class for the language model family of classes.
#'
#' This abstract class defines a common interface and methods of the LMStudio
#' family of classes.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family LMStudio Classes
#' @export
LMStudio0 <- R6::R6Class(
  classname = "LMStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..lm = character(),
    ..document = character(),
    ..tables = list(),
    ..size = integer(),
    ..modelType = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram')
  ),

  public = list(
    initialize = function(x) { stop("Not implemented for this abstract/interface class.") },
    build = function() { stop("Not implemented for this abstract/interface class.") }
  )
)
