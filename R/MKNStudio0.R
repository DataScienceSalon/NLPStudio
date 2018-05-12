#==============================================================================#
#                               MKNStudio0                                     #
#==============================================================================#
#' MKNStudio0
#'
#' \code{MKNStudio0} Abstract class for the Modified Kneser Ney language model state classes.
#'
#' This abstract class defines a common interface and methods of the
#' Modified Kneser Ney language model state classes.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNStudio0 <- R6::R6Class(
  classname = "MKNStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = LMStudio0,

  private = list(
    ..regex = list(
      prefix = list(
        bigrams   = "^((\\S+\\s+){0}\\S+).*$",
        trigrams  = "^((\\S+\\s+){1}\\S+).*$",
        quadgrams = "^((\\S+\\s+){2}\\S+).*$"
      ),
      suffix = list(
        bigrams = "^.*\\s+((?:\\S+\\s+){0}\\S+)$",
        trigrams = "^.*\\s+((?:\\S+\\s+){1}\\S+)$",
        quadgrams = "^.*\\s+((?:\\S+\\s+){2}\\S+)$"
      )
    )
  ),

  public = list(
    initialize = function(x) { stop("Not implemented for this abstract/interface class.") },
    build = function() { stop("Not implemented for this abstract/interface class.") }
  )
)
