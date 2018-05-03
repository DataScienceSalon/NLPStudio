#------------------------------------------------------------------------------#
#                         Replace Internet Slang App                           #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlangApp
#'
#' \code{ReplaceInternetSlangApp}  Replace Internet Slang
#'
#' A wrapper for \code{\link[textclean]{replace_internet_slang}}
#' replaces internet slang.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceInternetSlangApp$new(x, slang = NULL, replacement = NULL, ignoreCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param slang A vector of slang strings to replace.
#' @param replacement A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case).
#' Applies to default internet slang only.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceInternetSlangApp} Returns a vector with internet slang replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceInternetSlangApp <- R6::R6Class(
  classname = "ReplaceInternetSlangApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical(),

    processDocument = function(document) {
      document$text <- textclean::replace_internet_slang(x = document$text,
                                         slang = private$..slang,
                                         replacement = private$..replacement,
                                         ignore.case = private$..ignoreCase)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, slang = NULL, replacement = NULL, ignoreCase = TRUE) {

      private$loadDependencies()

      # Validate parameters# Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$kv$key <- slang
      private$..params$kv$value <- replacement
      private$..params$kv$equalLen <- TRUE
      private$..params$logicals$variables <- c('ignoreCase')
      private$..params$logicals$values <- c(ignoreCase)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..slang <- slang
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase

      invisible(self)
    }
  )
)
