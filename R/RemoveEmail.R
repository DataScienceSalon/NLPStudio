#------------------------------------------------------------------------------#
#                            RemoveEmail                                       #
#------------------------------------------------------------------------------#
#' RemoveEmail
#'
#' \code{RemoveEmail} Command for the RemoveEmail class.
#'
#' Class that encapsulates the command to execute an object of the RemoveEmail
#' class
#'
#' @usage RemoveEmail$new()
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
RemoveEmail <- R6::R6Class(
  classname = "RemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveEmailApp$new(x)$execute()
      return(x)
    }
  )
)
