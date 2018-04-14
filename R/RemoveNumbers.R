#------------------------------------------------------------------------------#
#                            RemoveNumbers                                     #
#------------------------------------------------------------------------------#
#' RemoveNumbers
#'
#' \code{RemoveNumbers} Command for the RemoveNumbers class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNumbers
#' class
#'
#' @usage RemoveNumbers$new()
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
RemoveNumbers <- R6::R6Class(
  classname = "RemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNumbersApp$new(x)$execute()
      return(x)
    }
  )
)
