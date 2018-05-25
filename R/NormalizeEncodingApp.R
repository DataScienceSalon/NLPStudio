#------------------------------------------------------------------------------#
#                         Normalize Encoding App                               #
#------------------------------------------------------------------------------#
#' NormalizeEncodingApp
#'
#' \code{NormalizeEncodingApp}  Declares and converts encoding to UTF-8.
#'
#' @usage NormalizeEncodingApp$new(x)$execute()
#'
#' @template textStudioParams
#' @param encoding Character string indicating the type of encoding. Valid
#' values are c('latin1', 'UTF-8', 'bytes')
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @return \code{NormalizeEncodingApp} Returns vectors of text in UTF-8 encoded format.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
NormalizeEncodingApp <- R6::R6Class(
  classname = "NormalizeEncodingApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..encoding = character(),

    processDocument = function(document) {

      content <- document$content
      Encoding(content) <- private$..encoding
      content <- enc2utf8(content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")
      document$content <- content
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, encoding = 'latin1') {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$discrete$variables <- c('encoding')
      private$..params$discrete$values <- list(encoding)
      private$..params$discrete$valid <-  list(c('latin1', 'UTF-8', 'bytes'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..encoding <- encoding
      invisible(self)
    }
  )
)
