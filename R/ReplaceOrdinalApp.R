#------------------------------------------------------------------------------#
#                             Replace Ordinal App                              #
#------------------------------------------------------------------------------#
#' ReplaceOrdinalApp
#'
#' \code{ReplaceOrdinalApp}  Replace Mixed Ordinal Numbers With Text Representation
#'
#' A wrapper for \code{\link[textclean]{replace_ordinal}} Replaces mixed text/numeric
#' represented ordinal numbers with words (e.g., "1st" becomes "first").
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceOrdinalAppApp$new(x)$execute()
#' @usage ReplaceOrdinalApp$new(x, joinOrdinal = TRUE, remove = FALSE)$execute()
#'
#' @template textStudioParams
#' @param joinOrdinal Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceOrdinalApp} Returns a vector with ordinals replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceOrdinalApp <- R6::R6Class(
  classname = "ReplaceOrdinalApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..joinOrdinal = logical(),
    ..remove = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_number(x = document$content,
                                            num.paste = private$..joinOrdinal,
                                            remove = private$..remove)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, joinOrdinal = FALSE, remove = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('joinOrdinal', 'remove')
      private$..params$logicals$values <- c(joinOrdinal, remove)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..joinOrdinal <- joinOrdinal
      private$..remove <- remove

      invisible(self)
    }
  )
)
