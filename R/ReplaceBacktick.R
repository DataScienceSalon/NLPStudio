#------------------------------------------------------------------------------#
#                              ReplaceBacktick                                 #
#------------------------------------------------------------------------------#
#' ReplaceBacktick
#'
#' \code{ReplaceBacktick} Command for the ReplaceBacktick class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceBacktick
#' class
#'
#' @usage ReplaceBacktick$new()
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
ReplaceBacktick <- R6::R6Class(
  classname = "ReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,


  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceBacktickApp$new(x)$execute()
      return(x)
    }
  )
)
