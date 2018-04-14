#------------------------------------------------------------------------------#
#                              Replace Backtick                                #
#------------------------------------------------------------------------------#
#' ReplaceBacktickApp
#'
#' \code{ReplaceBacktickApp} Replace backtick
#'
#' Replaces backticks with single quotes.
#'
#' @usage ReplaceBacktickAppApp$new(x)$execute()
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
ReplaceBacktickApp <- R6::R6Class(
  classname = "ReplaceBacktickApp",
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
      private$..pattern <- "\\`"
      private$..replacement <- "\\'"
      private$..ignoreCase <- FALSE
      private$..perl <- FALSE
      private$..fixed <- FALSE

      invisible(self)
    }
  )
)
