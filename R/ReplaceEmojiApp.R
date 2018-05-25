#------------------------------------------------------------------------------#
#                         Replace Emoji App                                    #
#------------------------------------------------------------------------------#
#' ReplaceEmojiApp
#'
#' \code{ReplaceEmojiApp}  Replace emojis with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emoji}} that replaces
#' emojis with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmojiApp$new(x, emojis = NULL)$execute()
#'
#' @template textStudioParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmojiApp} Returns a vector with emojis replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmojiApp <- R6::R6Class(
  classname = "ReplaceEmojiApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emojis = data.table(),

    processDocument = function(document) {
      content <- document$content
      if (is.null(private$..emojis)) {
        document$content <- textclean::replace_emoji(x = content)
      } else {
        document$content <- textclean::replace_emoji(x = content,
                                            emoji_dt = private$..emojis)
      }
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, emojis = NULL) {

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
      private$..emojis <- emojis

      invisible(self)
    }
  )
)
