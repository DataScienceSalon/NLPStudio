#------------------------------------------------------------------------------#
#                         Replace HTML App                                     #
#------------------------------------------------------------------------------#
#' ReplaceHTMLApp
#'
#' \code{ReplaceHTMLApp}  Replace HTML Markup
#'
#' A wrapper for \code{\link[textclean]{replace_html}} replaces HTML markup.
#' The angle braces are removed and the HTML symbol markup is replaced
#' with equivalent symbols.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceHTMLApp$new(x, symbol = TRUE)$execute()
#'
#' @template textStudioParams
#' @param symbol Logical. If codeTRUE the symbols are retained with appropriate replacements.
#' If FALSE they are removed.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceHTMLApp} Returns a vector with HTML markup replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceHTMLApp <- R6::R6Class(
  classname = "ReplaceHTMLApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..symbol = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_html(x = document$content,
                                             symbol = private$..symbol)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, symbol = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('symbol')
      private$..params$logicals$values <- c(symbol)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }


      private$..x <- x
      private$..symbol <- symbol

      invisible(self)
    }
  )
)
