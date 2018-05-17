#' CSource
#'
#' \code{CSource} Class for sourcing Corpus objects.
#'
#' CSource is the client facing class used to source a Corpus object from a variety
#' of formats. Methods support sourcing Corpora objects from character vectors,
#' JSON, and XML files or directories. Corpora may be imported from other
#' packages such as Quanteda, TM, KoRpus, and qdap.
#'
#' @usage myCorpus <- CSource$new(x)$vector()
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the CSource class.}
#'   \item{\code{vector(x, name = NULL)}}{Sources the Corpus object from a character vector.}
#'   \item{\code{directory(x, name = NULL)}}{Sources the Corpus object from a directory.}
#'   \item{\code{json(x, name = NULL)}}{Sources the Corpus object from a json source.}
#'   \item{\code{xml(x, name = NULL)}}{Sources the Corpus object from an xml source.}
#'  }
#'
#' @param x Character vector containing text, the name of a file, or directory
#' or a corpus object from a supported package.
#' @param name Character vector containing the name to assign to the Corpus object.
#' @param concatenate Logical for the vector method. If TRUE, character vectors are
#' concatenated into a single text for the Document object.
#'
#' @return Corpus object.
#'
#' @examples
#' corpus <- CSource$new()$vector(x)
#' corpus <- CSource$new()$file(x)
#' corpus <- CSource$new()$directory(x)
#' corpus <- CSource$new()$json(x)
#' corpus <- CSource$new()$xml(x)
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSource <- R6::R6Class(
  classname = "CSource",
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
    vector = function(x, name = NULL, collapse = FALSE) {
      corpus <- CSourceVector$new()$source(x = x, name = name,
                                           collapse = collapse)
      return(corpus)
    },

    dir = function(x, name = NULL,  docNames = TRUE, repair = TRUE) {
      corpus <- CSourceDir$new()$source(x = x, name = name,
                                        docNames, repair)
      return(corpus)
    },

    json = function(x, name = NULL) {
      stop("This method is currently unsupported")
    },

    xml = function(x, name = NULL) {
      stop("This method is currently unsupported")
    },

    quanteda = function(x, name = NULL) {
      corpus <- CSourceQuanteda$new()$source(x = x, name = name)
      return(corpus)
    },

    tm = function(x, name = NULL) {
      corpus <- CSourceTM$new()$source(x = x, name = name)
      return(corpus)
    },

    korpus = function(x, name = NULL) {
      corpus <- CSourceKorpus$new()$source(x = x, name = name)
      return(corpus)
    },

    qdap = function(x, name = NULL) {
      corpus <- CSourceQdap$new()$source(x = x, name = name)
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csource(self)
    }
  )
)
