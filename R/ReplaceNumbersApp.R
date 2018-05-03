#------------------------------------------------------------------------------#
#                             Replace Numbers App                              #
#------------------------------------------------------------------------------#
#' ReplaceNumbersApp
#'
#' \code{ReplaceNumbersApp}  Replace Numbers With Text Representation.
#'
#' A wrapper for \code{\link[textclean]{replace_number}} Replaces numeric represented numbers with words (e.g., 1001 becomes one thousand one).
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNumbersAppApp$new(x)$execute()
#' @usage ReplaceNumbersApp$new(x, joinNumbers = TRUE, remove = FALSE)$execute()
#'
#' @template textStudioParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNumbersApp} Returns a vector with numbers replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNumbersApp <- R6::R6Class(
  classname = "ReplaceNumbersApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..joinNumbers = logical(),
    ..remove = logical(),

    processDocument = function(document) {
      document$text <- textclean::replace_number(x = document$text,
                                            num.paste = private$..joinNumbers,
                                            remove = private$..remove)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, joinNumbers = FALSE, remove = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('joinNumbers', 'remove')
      private$..params$logicals$values <- c(joinNumbers, remove)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove

      invisible(self)
    }
  )
)
