#------------------------------------------------------------------------------#
#                            RemoveHyphens                                     #
#------------------------------------------------------------------------------#
#' RemoveHyphens
#'
#' \code{RemoveHyphens} Command for the RemoveHyphens class.
#'
#' Class that encapsulates the command to execute an object of the RemoveHyphens
#' class
#'
#' @usage RemoveHyphens$new()
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
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveHyphensApp$new(x)$execute()
      return(x)
    }
  )
)
