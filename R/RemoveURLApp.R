#------------------------------------------------------------------------------#
#                              Remove URL App                                  #
#------------------------------------------------------------------------------#
#' RemoveURLApp
#'
#' \code{RemoveURLApp} Removes URLs.
#'
#' Removes URLs  from text.
#'
#' @usage RemoveURLApp$new(x)$execute()
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
RemoveURLApp <- R6::R6Class(
  classname = "RemoveURLApp",
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
      private$..pattern <- "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)"
      private$..replacement = " "
      private$..ignoreCase <- TRUE
      private$..perl <- TRUE
      private$..fixed <- FALSE

      invisible(self)
    }
  )
)
