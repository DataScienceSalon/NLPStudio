#------------------------------------------------------------------------------#
#                          Remove Twitter App                                  #
#------------------------------------------------------------------------------#
#' RemoveTwitterApp
#'
#' \code{RemoveTwitterApp} Removes twitter.
#'
#' Removes twitter handles from text.
#'
#' @usage RemoveTwitterAppApp$new(x)$execute()
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
RemoveTwitterApp <- R6::R6Class(
  classname = "RemoveTwitterApp",
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
      private$..pattern <- '\\B#\\w*[a-zA-Z]+\\w*'
      private$..replacement <- " "
      private$..ignoreCase <- TRUE
      private$..perl <- TRUE
      private$..fixed <- FALSE

      invisible(self)
    }
  )
)
