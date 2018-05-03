#------------------------------------------------------------------------------#
#                       Replace Curly Quotes App                               #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotesApp
#'
#' \code{ReplaceCurlyQuotesApp}  Replaces curly single and double quotes.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces curly single and double quotes.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceCurlyQuotesApp$new(x, removeNonCoverted)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceCurlyQuotesApp} Returns a vector with curly quotes replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceCurlyQuotesApp <- R6::R6Class(
  classname = "ReplaceCurlyQuotesApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    processDocument = function(document) {
      content <- document$text
      Encoding(document$text) <- "latin1"
      document$text <- textclean::replace_curly_quote(x = content)
      private$logEvent(document)
      return(document)
    }
  ),

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

      invisible(self)
    }
  )
)
