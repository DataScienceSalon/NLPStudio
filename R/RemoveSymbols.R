#------------------------------------------------------------------------------#
#                            RemoveSymbols                                     #
#------------------------------------------------------------------------------#
#' RemoveSymbols
#'
#' \code{RemoveSymbols} Command for the RemoveSymbols class.
#'
#' Class that encapsulates the command to execute an object of the RemoveSymbols
#' class
#'
#' @usage RemoveSymbols$new()
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
RemoveSymbols <- R6::R6Class(
  classname = "RemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,


  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveSymbolsApp$new(x)$execute()
      return(x)
    }
  )
)
