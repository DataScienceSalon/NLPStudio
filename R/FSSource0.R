#' FSSource0
#'
#' \code{FSSource0} Abstraction for the classes responsible for sourcing FileSet objects.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract.}
#'   \item{\code{source(x, name = NULL)}}{Executes the process of sourcing the FileSet object.}
#'  }
#'
#' @param name Optional character vector indicating name for FileSet object.
#' @param x Character vector containing text, the name of a file, or directory
#' from which the FileSet object will be sourced.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Set Source Classes
#' @export
FSSource0 <- R6::R6Class(
  classname = "FSSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..fileSet = character()
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this abstract class") },
    source = function() { stop("This method is not implemented for this abstract class") }
  )
)
