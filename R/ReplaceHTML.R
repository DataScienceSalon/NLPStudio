#------------------------------------------------------------------------------#
#                                ReplaceHTML                                   #
#------------------------------------------------------------------------------#
#' ReplaceHTML
#'
#' \code{ReplaceHTML} Command for the ReplaceHTML class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceHTML
#' class
#'
#' @usage ReplaceHTML$new(symbol = FALSE)
#'
#' @template textStudioParams
#' @param symbol Logical. If codeTRUE the symbols are retained with appropriate replacements.
#' If FALSE they are removed.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceHTML <- R6::R6Class(
  classname = "ReplaceHTML",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..symbol = logical()
  ),

  public = list(
    initialize = function(symbol = TRUE) {
      private$loadDependencies()
      private$..symbol <- symbol
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceHTMLApp$new(x, private$..symbol)$execute()
      return(x)
    }
  )
)
