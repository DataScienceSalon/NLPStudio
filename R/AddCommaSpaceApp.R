#------------------------------------------------------------------------------#
#                              Add Comma Space App                             #
#------------------------------------------------------------------------------#
#' AddCommaSpaceApp
#'
#' \code{AddCommaSpaceApp} Adds space after comma.
#'
#' Class adds space after comma when commas are followed by non space characters.
#'
#' @usage AddCommaSpaceApp$new(x)$execute()
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
AddCommaSpaceApp <- R6::R6Class(
  classname = "AddCommaSpaceApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {
      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..regex <- "(,)([^ ])"
      private$..replacement <- "\\1 \\2"
      invisible(self)
    }
  )
)
