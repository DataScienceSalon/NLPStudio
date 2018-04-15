#------------------------------------------------------------------------------#
#                                  ReplaceOrdinal                              #
#------------------------------------------------------------------------------#
#' ReplaceOrdinal
#'
#' \code{ReplaceOrdinal} Command for the ReplaceOrdinal class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceOrdinal
#' class
#'
#' @usage ReplaceOrdinal$new(joinOrdinal = FALSE, remove = FALSE)
#'
#' @template textStudioParams
#' @param joinOrdinal Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceOrdinal <- R6::R6Class(
  classname = "ReplaceOrdinal",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..joinOrdinal = logical(),
    ..remove = logical()
  ),

  public = list(
    initialize = function(numPaste = FALSE, remove = FALSE) {
      private$loadDependencies()

      # Validate parameters
      private$..params$logicals$variables <- c('numPaste', 'remove')
      private$..params$logicals$values <- c(numPaste, remove)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }
      private$..numPaste <- numPaste
      private$..remove <- remove
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceOrdinalApp$new(x, numPaste = private$..numPaste,
                              remove = private$..remove)$execute()
      return(x)
    }
  )
)
