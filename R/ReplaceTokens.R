#------------------------------------------------------------------------------#
#                              ReplaceTokens                             #
#------------------------------------------------------------------------------#
#' ReplaceTokens
#'
#' \code{ReplaceTokens} Command for the ReplaceTokens class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceTokens
#' class
#'
#' @usage ReplaceTokens$new(tokens = NULL, ignoreCase = TRUE)
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
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceTokens <- R6::R6Class(
  classname = "ReplaceTokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..tokens = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = logical()
  ),

  public = list(
    initialize = function(tokens = NULL, replacement = NULL, leadspace = FALSE,
                          trailspace = FALSE, trim = FALSE) {
      private$loadDependencies()

      # Validate parameters
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

      private$..tokens <- tokens
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..trim <- trim
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceTokensApp$new(x, tokens = private$..tokens,
                                   replacement = private$..replacement,
                                   leadspace = private$..leadspace,
                                   trailspace = private$..trailspace,
                                   trim = private$..trim)$execute()
      return(x)
    }
  )
)
