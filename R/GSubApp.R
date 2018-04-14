#------------------------------------------------------------------------------#
#                                   GSub App                                   #
#------------------------------------------------------------------------------#
#' GSubApp
#'
#' \code{GSubApp} Removes email addresses from text.
#'
#' @usage GSubAppApp$new(x)$execute()
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
GSubApp <- R6::R6Class(
  classname = "GSubApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x, pattern, replacement, ignoreCase = FALSE,
                          perl = FALSE, fixed = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('ignoreCase', 'perl', 'fixed')
      private$..params$logicals$values <- c(ignoreCase, perl, fixed)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..pattern <- pattern
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..perl <- perl
      private$..fixed <- fixed

      invisible(self)
    }
  )
)
