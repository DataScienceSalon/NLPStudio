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
    ..model = character(),
    ..regex = list(
      prefix = list(
        bigrams   = "^((\\S+\\s+){0}\\S+).*$",
        trigrams  = "^((\\S+\\s+){1}\\S+).*$",
        quadgrams = "^((\\S+\\s+){2}\\S+).*$",
        quintgrams = "^((\\S+\\s+){3}\\S+).*$"
      ),
      suffix = list(
        bigrams = "^.*\\s+((?:\\S+\\s+){0}\\S+)$",
        trigrams = "^.*\\s+((?:\\S+\\s+){1}\\S+)$",
        quadgrams = "^.*\\s+((?:\\S+\\s+){2}\\S+)$",
        quintgrams = "^.*\\s+((?:\\S+\\s+){3}\\S+)$"
      )
    )
  ),

  public = list(
    initialize = function(x) { stop("Not implemented for this abstract/interface class.") },
    build = function() { stop("Not implemented for this abstract/interface class.") }
  )
)
