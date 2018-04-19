#' FCSource0
#'
#' \code{FCSource0} Abstraction for the classes responsible for sourcing FileCollection objects.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract.}
#'   \item{\code{source(x, name = NULL)}}{Executes the process of sourcing the FileCollection object.}
#'  }
#'
#' @param name Optional character vector indicating name for FileCollection object.
#' @param x Character vector containing text, the name of a file, or directory
#' from which the FileCollection object will be sourced.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Collection Source Classes
#' @export
FCSource0 <- R6::R6Class(
  classname = "FCSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..fileCollection = character()
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this abstract class") },
    source = function() { stop("This method is not implemented for this abstract class") }
  )
)
