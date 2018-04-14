#------------------------------------------------------------------------------#
#                               RemoveWhiteSpace                               #
#------------------------------------------------------------------------------#
#' RemoveWhiteSpace
#'
#' \code{RemoveWhiteSpace} Command for the RemoveWhiteSpace class.
#'
#' Class that encapsulates the command to execute an object of the RemoveWhiteSpace
#' class
#'
#' @usage RemoveWhiteSpace$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveWhiteSpace <- R6::R6Class(
  classname = "RemoveWhiteSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveWhiteSpaceApp$new(x)$execute()
      return(x)
    }
  )
)
