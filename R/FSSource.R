#' FSSource
#'
#' \code{FSSource} Class for sourcing FileSet objects.
#'
#' FSSource is the client facing class used to source a FileSet object from a variety
#' of formats. Methods support sourcing Corpora objects from character vectors,
#' JSON, and XML files or directories. Corpora may be imported from other
#' packages such as Quanteda, TM, KoRpus, and qdap.
#'
#' @usage myFileSet <- FSSource$new(x)$vector()
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the FSSource class.}
#'   \item{\code{vector(x, name = NULL)}}{Sources the FileSet object from a character vector.}
#'   \item{\code{directory(x, name = NULL)}}{Sources the FileSet object from a directory.}
#'   \item{\code{json(x, name = NULL)}}{Sources the FileSet object from a json source.}
#'   \item{\code{xml(x, name = NULL)}}{Sources the FileSet object from an xml source.}
#'  }
#'
#' @param x Character vector containing text, the name of a file, or directory
#' or a FileSet object from a supported package.
#' @param name Character vector containing the name to assign to the FileSet object.
#' @param concatenate Logical for the vector method. If TRUE, character vectors are
#' concatenated into a single text for the Document object.
#'
#' @return FileSet object.
#'
#' @examples
#' FileSet <- FSSource$new()$vector(x)
#' FileSet <- FSSource$new()$file(x)
#' FileSet <- FSSource$new()$directory(x)
#' FileSet <- FSSource$new()$json(x)
#' FileSet <- FSSource$new()$xml(x)
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Set Source Classes
#' @export
FSSource <- R6::R6Class(
  classname = "FSSource",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() { invisible(self) },

    #-------------------------------------------------------------------------#
    #                            Source Methods                               #
    #-------------------------------------------------------------------------#

    dir = function(x, name = NULL, docNames = TRUE) {
      fileSet <- FSSourceDir$new(x = x, name = name, docNames)$source()
      return(fileSet)
    },

    json = function(x, name = NULL) {
      stop("This method is currently unsupported")
    },

    xml = function(x, name = NULL) {
      stop("This method is currently unsupported")
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fcsource(self)
    }
  )
)
