#------------------------------------------------------------------------------#
#                                  ReplaceKern                                 #
#------------------------------------------------------------------------------#
#' ReplaceKern
#'
#' \code{ReplaceKern} Command for the ReplaceKern class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceKern
#' class
#'
#' @usage ReplaceKern$new()
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
ReplaceKern <- R6::R6Class(
  classname = "ReplaceKern",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceKernApp$new(x)$execute()
      return(x)
    }
  )
)
