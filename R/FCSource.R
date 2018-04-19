#' FCSource
#'
#' \code{FCSource} Class for sourcing FileCollection objects.
#'
#' FCSource is the client facing class used to source a FileCollection object from a variety
#' of formats. Methods support sourcing Corpora objects from character vectors,
#' JSON, and XML files or directories. Corpora may be imported from other
#' packages such as Quanteda, TM, KoRpus, and qdap.
#'
#' @usage myFileCollection <- FCSource$new(x)$vector()
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the FCSource class.}
#'   \item{\code{vector(x, name = NULL)}}{Sources the FileCollection object from a character vector.}
#'   \item{\code{directory(x, name = NULL)}}{Sources the FileCollection object from a directory.}
#'   \item{\code{json(x, name = NULL)}}{Sources the FileCollection object from a json source.}
#'   \item{\code{xml(x, name = NULL)}}{Sources the FileCollection object from an xml source.}
#'  }
#'
#' @param x Character vector containing text, the name of a file, or directory
#' or a FileCollection object from a supported package.
#' @param name Character vector containing the name to assign to the FileCollection object.
#' @param concatenate Logical for the vector method. If TRUE, character vectors are
#' concatenated into a single text for the Document object.
#'
#' @return FileCollection object.
#'
#' @examples
#' FileCollection <- FCSource$new()$vector(x)
#' FileCollection <- FCSource$new()$file(x)
#' FileCollection <- FCSource$new()$directory(x)
#' FileCollection <- FCSource$new()$json(x)
#' FileCollection <- FCSource$new()$xml(x)
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Collection Source Classes
#' @export
FCSource <- R6::R6Class(
  classname = "FCSource",
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
      fileCollection <- FCSourceDir$new(x = x, name = name, docNames)$source()
      return(fileCollection)
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
