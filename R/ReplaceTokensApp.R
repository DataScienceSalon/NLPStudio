#------------------------------------------------------------------------------#
#                            Replace Tokens App                                #
#------------------------------------------------------------------------------#
#' ReplaceTokensApp
#'
#' \code{ReplaceTokensApp}  Replace tokens.
#'
#' A wrapper for \code{\link[textclean]{mgsub}}, A wrapper for gsub that takes
#' a vector of search terms and a vector or single value of replacements.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceTokensApp$new(x, tokens, replacement, leadspace, trailspace, fixed, trim, orderPattern)$execute()
#'
#' @template textStudioParams
#' @param tokens Character string of tokens to be matched in the
#' given character vector.
#' @param replacement Character string equal in length to tokens containing
#'  the long forms of the tokens.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, tokens is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param orderPattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{tokens} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{tokens = c("the", "then")} resorts to
#' search for "then" first).
#' @param \dots ignored.
#'
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceTokensApp} Returns a vector with tokens replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceTokensApp <- R6::R6Class(
  classname = "ReplaceTokensApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..tokens = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character(),

    processDocument = function(document) {

      content <- document$text

      document$text <- textclean::mgsub(x = content,
                                  pattern = private$..tokens,
                                  replacement = private$..replacement,
                                  leadspace = private$..leadspace,
                                  trailspace = private$..trailspace,
                                  fixed = FALSE,
                                  trim = private$..trim,
                                  order.pattern = TRUE)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, tokens = NULL, replacement = NULL, leadspace = FALSE,
                          trailspace = FALSE, trim = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$kv$key <- tokens
      private$..params$kv$value <- replacement
      private$..params$kv$equalLen <- TRUE
      private$..params$logicals$variables <- c('leadSpace', 'trailspace', 'trim')
      private$..params$logicals$values <- c(leadspace, trailspace, trim)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      if ('data.frame' %in% class(tokens)) {
        if (ncol(tokens) == 2) {
          private$..tokens <- paste("\\b",as.character(tokens[,1]), "\\b", sep = "")
          private$..replacement <- as.character(tokens[,2])
        } else {
          private$..tokens <- paste("\\b",as.character(tokens[,1]), "\\b", sep = "")
          private$..replacement <- replacement
        }
      } else {
        private$..tokens <- paste("\\b",as.character(tokens), "\\b", sep = "")
        private$..replacement <- replacement
      }
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..trim <- trim

      invisible(self)
    }
  )
)
