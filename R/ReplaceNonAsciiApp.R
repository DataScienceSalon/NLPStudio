#------------------------------------------------------------------------------#
#                             Replace NonAscii App                             #
#------------------------------------------------------------------------------#
#' ReplaceNonAsciiApp
#'
#' \code{ReplaceNonAsciiApp}  Replace Common Non-ASCII Characters.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces common non-ascii characters.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNonAsciiAppApp$new(x)$execute()
#' @usage ReplaceNonAsciiApp$new(x, removeNonConverted = FALSE)$execute()
#'
#' @template textStudioParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNonAsciiApp} Returns a vector with non-ascii characters replaced
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNonAsciiApp <- R6::R6Class(
  classname = "ReplaceNonAsciiApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..removeNonConverted = logical(),

    processDocument = function(document) {

      content <- document$content

      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")

      document$content <- textclean::replace_non_ascii(x = content,
                                                       remove.nonconverted =
                                                         private$..removeNonConverted)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, removeNonConverted = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('removeNonConverted')
      private$..params$logicals$values <- c(removeNonConverted)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..removeNonConverted <- removeNonConverted

      invisible(self)
    }
  )
)
