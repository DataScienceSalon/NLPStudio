#------------------------------------------------------------------------------#
#                              ReplaceContractions                             #
#------------------------------------------------------------------------------#
#' ReplaceContractions
#'
#' \code{ReplaceContractions} Command for the ReplaceContractions class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceContractions
#' class
#'
#' @usage ReplaceContractions$new(contractions = NULL, ignoreCase = TRUE)
#'
#' @template textStudioParams
#' @param contractions Character string of contractions to be matched in the
#' given character vector. If NULL, the default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param replacement Character string equal in length to contractions containing
#'  the long forms of the contractions.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, contractions is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param orderPattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{contractions} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{contractions = c("the", "then")} resorts to
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
ReplaceContractions <- R6::R6Class(
  classname = "ReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..contractions = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = logical()
  ),

  public = list(
    initialize = function(contractions = NULL, replacement = NULL, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          orderPattern = fixed) {
      private$loadDependencies()
      private$..contractions <- contractions
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceContractionsApp$new(x, contractions = private$..contractions,
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
