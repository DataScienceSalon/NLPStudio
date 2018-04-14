#------------------------------------------------------------------------------#
#                            AddCommaSpace                                     #
#------------------------------------------------------------------------------#
#' AddCommaSpace
#'
#' \code{AddCommaSpace} Command for the AddCommaSpaceApp class.
#'
#' Class that encapsulates the command to execute an object of the AddCommaSpace
#' class
#'
#' @usage AddCommaSpace$new()
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
AddCommaSpace <- R6::R6Class(
  classname = "AddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {

      private$loadDependencies()

      invisible(self)
    },
    execute = function(x) {
      x <- AddCommaSpaceApp$new(x)$execute()
      return(x)
    }
  )
)
