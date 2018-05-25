#------------------------------------------------------------------------------#
#                         Replace Word Elongation App                          #
#------------------------------------------------------------------------------#
#' ReplaceWhiteApp
#'
#' \code{ReplaceWhiteApp}  Replace multiple white spaces with single space.
#'
#' A wrapper for \code{\link[textclean]{replace_white}} Replaces multiple whitespaces with a single space.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceWhiteApp$new(x, impartMeaning = FALSE)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceWhiteApp} Returns a vector with word elongations replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceWhiteApp <- R6::R6Class(
  classname = "ReplaceWhiteApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    processDocument = function(document) {
      document$content <- textclean::replace_white(x = document$content)
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
