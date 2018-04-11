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

      private$..corpus <- Corpus$new()

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL, collapse = FALSE) {

      # Validate text
      if (!is.null(x)) {
        private$..params <- list()
        private$..params$classes$name <- list('x')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid <- list('character', 'list')
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'source',
                           event = v$msg, level = "Error")
          stop()
        }
      }

      # Name the corpus
      if (!is.null(name)) {
        private$..corpus$setMeta(key = 'name', value = name)
      }

      if ("list" %in% class(x)[1]) {
        lapply(x, function(y) {
          name <- names(y)
          doc <- Document$new(x = y, name = name)
          private$..corpus$addDocument(x = doc)
        })
      } else if (collapse) {
        x <- paste0(x, collapse = " ")
        doc <- Document$new(x = x, name = name)
        private$..corpus$addDocument(x = doc)
      } else {
        docNames <- names(x)
        for (i in 1:length(x)) {
          if (is.null(name)) {
            name <- paste0("Document-",i)
          }
          doc <- Document$new(x = x[i], name = name)
          private$..corpus$addDocument(doc)
        }
      }

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceVector(self)
    }
  )
)
