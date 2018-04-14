#------------------------------------------------------------------------------#
#                               RemoveTwitter                                  #
#------------------------------------------------------------------------------#
#' RemoveTwitter
#'
#' \code{RemoveTwitter} Command for the RemoveTwitter class.
#'
#' Class that encapsulates the command to execute an object of the RemoveTwitter
#' class
#'
#' @usage RemoveTwitter$new()
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
RemoveTwitter <- R6::R6Class(
  classname = "RemoveTwitter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveTwitterApp$new(x)$execute()
      return(x)
    }
  )
)
