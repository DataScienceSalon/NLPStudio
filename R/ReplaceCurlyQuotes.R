#------------------------------------------------------------------------------#
#                              ReplaceCurlyQuotes                              #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotes
#'
#' \code{ReplaceCurlyQuotes} Command for the ReplaceCurlyQuotes class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceCurlyQuotes
#' class
#'
#' @usage ReplaceCurlyQuotes$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceCurlyQuotes <- R6::R6Class(
  classname = "ReplaceCurlyQuotes",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceCurlyQuotesApp$new(x)$execute()
      return(x)
    }
  )
)
