#------------------------------------------------------------------------------#
#                              ReplaceAbbreviations                            #
#------------------------------------------------------------------------------#
#' ReplaceAbbreviations
#'
#' \code{ReplaceAbbreviations} Command for the ReplaceAbbreviations class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceAbbreviations
#' class
#'
#' @usage ReplaceAbbreviations$new(abbreviation, replacement = NULL, ignoreCase = TRUE )
#'
#' @template textStudioParams
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations.
#' Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Should case be ignored? Only applies to default dictionary.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceAbbreviations <- R6::R6Class(
  classname = "ReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..abbreviations = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(abbreviations = NULL, replacement = NULL, ignoreCase = TRUE) {
      private$loadDependencies()
      private$..abbreviations <- abbreviations
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceAbbreviationsApp$new(x, abbreviations = private$..abbreviations,
                                   replacement = private$..replacement,
                                   ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
