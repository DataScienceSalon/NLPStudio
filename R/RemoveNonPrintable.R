#------------------------------------------------------------------------------#
#                               RemoveNonPrintable                             #
#------------------------------------------------------------------------------#
#' RemoveNonPrintable
#'
#' \code{RemoveNonPrintable} Command for the RemoveNonPrintable class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNonPrintable
#' class
#'
#' @usage RemoveNonPrintable$new()
#'
#' @template fileStudioParams
#' @template fileStudioMethods
#' @template fileStudioClasses
#' @template fileStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Classes
#' @export
RemoveNonPrintable <- R6::R6Class(
  classname = "RemoveNonPrintable",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  public = list(
    initialize = function(codes = c(0,26)) {
      private$..codes <- codes
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNonPrintableApp$new(x, codes = private$..codes)$execute()
      return(x)
    }
  )
)
