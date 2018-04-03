#------------------------------------------------------------------------------#
#                                Super Class                                   #
#------------------------------------------------------------------------------#
#' Super
#'
#' \code{Super} Base class for all classes.
#'
#' Class loads dependencies such as the validator and logging objects,
#' and provides access to a private member used to hold method parameter data
#' for validation.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract class.}
#'   \item{\code{getParams()}}{Returns the object's parameters.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Super <- R6::R6Class(
  classname = "Super",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..params = list(),

    logR =  character(),
    validator = character(),

    loadDependencies = function() {
      private$logR <- LogR$new()
      private$validator <- Validator$new()
      return(TRUE)
    }
  ),

  public = list(
    initialize = function() { stop("This method is not implemented for this base class.")},
    getId = function() { private$meta$getIdentity(key = "id") },
    getName = function() { private$meta$getIdentity(key = "name") },
    getParams = function() { private$..params }
  )
)
