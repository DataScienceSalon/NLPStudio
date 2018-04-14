#------------------------------------------------------------------------------#
#                            Replace Tokens App                                #
#------------------------------------------------------------------------------#
#' ReplaceTokensApp
#'
#' \code{ReplaceTokensApp}  Replace a vector of search tokens
#'
#' A wrapper for \code{\link[textclean]{mgsub}} that takes a vector
#' of search tokens and a vector or single value of replacements.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceTokensApp$new(x, pattern, replacement, leadspace = FALSE)$execute()
#'
#' @template textStudioParams
#' @param tokens Character string(s) to be matched in the given character vector.
#' @param replacement Character string equal in length to pattern or of length
#' one which are  a replacement for matched pattern.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param order.pattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{pattern} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to
#' search for "then" first).
#'
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @return \code{ReplaceTokensApp} - Returns a vector with the pattern replaced.
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

      document$content <- textclean::mgsub(x = document$content,
                                  pattern = private$..tokens,
                                  replacement = private$..replacement,
                                  leadspace = private$..leadspace,
                                  trailspace = private$..trailspace,
                                  fixed = private$..fixed,
                                  trim = private$..trim,
                                  order.pattern = private$..orderPattern)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, tokens, replacement, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          orderPattern = fixed) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$kv$key <- tokens
      private$..params$kv$value <- replacement
      private$..params$logicals$variables <- c('leadspace', 'trailspace', 'fixed', 'trim', 'orderPattern')
      private$..params$logicals$values <- c(leadspace, trailspace, fixed, trim, orderPattern)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x

      if (class(tokens)[1] == "data.frame") {
        private$..tokens <- as.character(tokens[,1])
        private$..replacement <- as.character(tokens[,2])
      } else {
        private$..tokens <- tokens
        private$..replacement <- replacement
      }
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern

      invisible(self)
    }
  )
)
