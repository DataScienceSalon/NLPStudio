#------------------------------------------------------------------------------#
#                                 ReplaceTokens                                #
#------------------------------------------------------------------------------#
#' ReplaceTokens
#'
#' \code{ReplaceTokens} Command for the ReplaceTokens class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceTokens
#' class
#'
#' @usage ReplaceTokens$new(joinTokens = FALSE, remove = FALSE)
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
    ..orderPattern = character()
  ),

  public = list(
    initialize = function(tokens, replacement = NULL, leadspace = FALSE, trailspace = FALSE,
                          fixed = TRUE, trim = FALSE, orderPattern = fixed) {
      private$loadDependencies()

      private$..tokens <- tokens
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceTokensApp$new(x, tokens = private$..tokens,
                            replacement = private$..replacement,
                            leadspace = private$..leadspace,
                            trailspace = private$..trailspace,
                            fixed = private$..fixed,
                            trim = private$..trim,
                            orderPattern = private$..orderPattern)$execute()
      return(x)
    }
  )
)
