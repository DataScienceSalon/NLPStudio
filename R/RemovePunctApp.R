#------------------------------------------------------------------------------#
#                          Remove Punctuation App                              #
#------------------------------------------------------------------------------#
#' RemovePunctApp
#'
#' \code{RemovePunctApp} Removes punctuation from text.
#'
#' @usage RemovePunctApp$new(x, endmark = FALSE, apostrophe = FALSE)$execute()
#'
#' @template textStudioParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemovePunctApp <- R6::R6Class(
  classname = "RemovePunctApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x, endmark = FALSE, apostrophe = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logical$variables <- c("endmark", "apostrophe")
      private$..params$logical$values <- c(endmark, apostrophe)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x

      if (endmark == FALSE & apostrophe == FALSE) {
        private$..regex <- "(?![.?!'])[[:punct:]]"
      } else if (endmark == FALSE) {
        private$..regex <- "(?![.?!])[[:punct:]]"
      } else if (apostrophe == FALSE) {
        private$..regex <- "(?!['])[[:punct:]]"
      } else {
        private$..regex <- "[[:punct:]]"
      }
      private$..replacement <- " "

      invisible(self)
    }
  )
)
