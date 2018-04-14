#------------------------------------------------------------------------------#
#                          Remove Email Addresses App                          #
#------------------------------------------------------------------------------#
#' RemoveEmailApp
#'
#' \code{RemoveEmailApp} Removes email addresses from text.
#'
#' @usage RemoveEmailAppApp$new(x)$execute()
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
RemoveEmailApp <- R6::R6Class(
  classname = "RemoveEmailApp",
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
      private$..regex <- "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}"

      invisible(self)
    }
  )
)
