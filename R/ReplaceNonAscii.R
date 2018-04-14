#------------------------------------------------------------------------------#
#                                  ReplaceNonAscii                             #
#------------------------------------------------------------------------------#
#' ReplaceNonAscii
#'
#' \code{ReplaceNonAscii} Command for the ReplaceNonAscii class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNonAscii
#' class
#'
#' @usage ReplaceNonAscii$new(removeNonConverted = FALSE)
#'
#' @template textStudioParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNonAscii <- R6::R6Class(
  classname = "ReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..removeNonConverted = logical()
  ),

  public = list(
    initialize = function(removeNonConverted = TRUE) {
      private$loadDependencies()
      private$..removeNonConverted <- removeNonConverted
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceNonAsciiApp$new(x, removeNonConverted = private$..removeNonConverted)$execute()
      return(x)
    }
  )
)
