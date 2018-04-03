#' Validator
#'
#' Class accepts validation requests from client objects and dispatches a
#' ValidationVisitor class object to perform validation.
#'
#' Class responsible for accepting validation requests and delegating
#' the request to a ValidationVisitor object.
#'
#' @section Validator methods:
#' This section summarizes the methods in the Validator class.
#' \describe{
#'  \item{\code{validate(object)}}{Dispatches the ValidationVisitor to the client object.}
#'  }
#'
#' @param object Object to be validated
#'
#' @return A list containing:
#' \itemize{
#'  \item status: Logical. If validation passed, then, TRUE, otherwise FALSE
#'  \item msg: Character string describing the error if status is FALSE
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator <- R6::R6Class(
  "Validator",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    validate = function(object) {
      visitor <- ValidationVisitor$new()
      object$accept(visitor)
    }
  )
)
