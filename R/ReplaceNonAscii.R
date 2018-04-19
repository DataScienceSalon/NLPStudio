#------------------------------------------------------------------------------#
#                                  RemoveNonPrintable                          #
#------------------------------------------------------------------------------#
#' RemoveNonPrintable
#'
#' \code{RemoveNonPrintable} Command for the RemoveNonPrintableApp class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNonPrintableApp
#' class
#'
#' @template textStudioParams
#' @param codes Vector of integers representing the decimal code for control
#' characters to be removed from the file.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveNonPrintable <- R6::R6Class(
  classname = "RemoveNonPrintable",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(
    ..codes = numeric()
  ),

  public = list(
    initialize = function(codes = setdiff(seq(0,31), 12)) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('codes')
      private$..params$classes$objects <- list(codes)
      private$..params$classes$valid <- list(c("numeric"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..codes <- codes
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNonPrintableApp$new(x, codes = private$..codes)$execute()
      return(x)
    }
  )
)
