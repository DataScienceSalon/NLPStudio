#------------------------------------------------------------------------------#
#                                RemoveURL                                     #
#------------------------------------------------------------------------------#
#' RemoveURL
#'
#' \code{RemoveURL} Command for the RemoveURL class.
#'
#' Class that encapsulates the command to execute an object of the RemoveURL
#' class
#'
#' @usage RemoveURL$new()
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
RemoveURL <- R6::R6Class(
  classname = "RemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveURLApp$new(x)$execute()
      return(x)
    }
  )
)
