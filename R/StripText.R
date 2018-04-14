#------------------------------------------------------------------------------#
#                                  StripText                                   #
#------------------------------------------------------------------------------#
#' StripText
#'
#' \code{StripText} Command for the StripText class.
#'
#' Class that encapsulates the command to execute an object of the StripText
#' class
#'
#' @usage StripText$new(keepChars = NULL, removeDigit = TRUE, lowerCase = TRUE)
#'
#' @template textStudioParams
#' @param keepChars A character vector of symbols (i.e., punctuation) that
#' strip should keep. The default is to strip every symbol except apostrophes
#' and a double tilde "~~". The double tilde "~~" is included for a
#' convenient means of keeping word groups together in functions that
#' split text apart based on space To remove double tildes "~~" set
#' char.keep to NULL.
#' @param removeDigit Logical. If TRUE, digits are removed from the text.
#' @param removeApostrophe Logical. If TRUE, digits are removed from the text.
#' @param lowerCase Logical.  If TRUE, forces all alphabetic characters to lower case.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
StripText <- R6::R6Class(
  classname = "StripText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..keepChars = character(),
    ..removeDigit = logical(),
    ..removeApostrophe = logical(),
    ..lowerCase = logical()
  ),

  public = list(
    initialize = function(keepChar = NULL, removeDigit = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$loadDependencies()
      private$..keepChar <- keepChar
      private$..removeDigit <- removeDigit
      private$..removeApostrophe <- removeApostrophe
      private$..lowerCase = lowerCase
      invisible(self)
    },
    execute = function(x) {
      x <- StripTextApp$new(x, keepChar = private$..keepChar,
                         removeDigit = private$..removeDigit,
                         removeApostrophe = private$..removeApostrophe,
                         lowerCase = private$..lowerCase)$execute()
      return(x)
    }
  )
)
