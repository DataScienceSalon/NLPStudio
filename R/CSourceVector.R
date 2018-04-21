#' CSourceVector
#'
#' \code{CSourceVector} Sources a Corpus object from a character vector or vectors.
#'
#' Sources a Corpus object from a character vector or vectors. Each vector element
#' is treated as a separate document unless the collapse paramter is set to TRUE.
#' If a list is encountered, each list element is treated as a separate document
#' and the vectors are collapsed into a single document within the list
#' structure.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(x, name = NULL)}}{Initializes an object of the CSourceVector class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#' @param collapse Logical. If true, collapse the character vectors into
#' a single document.
#'
#' @examples
#' text <- c("The firm possesses unparalleled leverage in Washington,
#' thanks in part to its track record of funneling executives into
#' senior government posts. Even the Trump administration, which
#' rode a populist wave to electoral victory, is stocked with
#' Goldman alumni, including Treasury Secretary Steven Mnuchin and
#' the departing White House economic adviser Gary D. Cohn.",
#' "Goldman is also an adviser to many of America’s — and the
#' world’s — largest companies, ranging from stalwarts like Walt
#' Disney to upstarts like Uber.")
#'
#' corpus <- CSource$new(x = txt, name = "Goldman")$vector()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceVector <- R6::R6Class(
  classname = "CSourceVector",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CSource0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL) {

      # Validate text
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('character', 'list'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'source',
                         event = v$msg, level = "Error")
        stop()
      }

      corpus <- Corpus$new(name = name)
      corpus$setMeta(key = 'source', value = 'Vector Source', type = 'f')

      # Create Documents and add to Corpus
      docNames <- names(x)
      if (is.null(docNames)) {
        docNames <- paste0("Document-", seq(1,length(x)))
      }
      for (i in 1:length(x)) {
        doc <- Document$new(x = x[[i]], name = docNames[i])
          if (!is.null(names(x))) {
            sourceVec <- paste0("Sourced from vector ", docNames[i])
            doc$setMeta(key = 'source', value = sourceVec, type = 'f')
          }
        corpus$addDocument(x = doc)
      }


      corpus <- private$sumQuant(corpus)

      event <- paste0("Corpus ", corpus$getName(), " instantiated from ",
                      "a vector source.")
      corpus$message(event = event)

      return(corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceVector(self)
    }
  )
)
