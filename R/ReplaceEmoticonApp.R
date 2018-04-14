#------------------------------------------------------------------------------#
#                         Replace Emoticons App                                #
#------------------------------------------------------------------------------#
#' ReplaceEmoticonApp
#'
#' \code{ReplaceEmoticonApp}  Replace emoticons with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emoticon}} that replaces
#' emoticons with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmoticonApp$new(x, emoticons = NULL)$execute()
#'
#' @template textStudioParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmoticonApp} Returns a vector with emoticons replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoticonApp <- R6::R6Class(
  classname = "ReplaceEmoticonApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emoticons = data.table(),

    processDocument = function(document) {
      content <- document$content
      if (is.null(private$..emoticons)) {
        document$content <- textclean::replace_emoticon(x = content)
      } else {
        document$content <- textclean::replace_emoticon(x = content,
                                               emoticon_dt = private$..emoticons)
      }
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, emoticons = NULL) {

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
      private$..emoticons <- emoticons

      invisible(self)
    }
  )
)
