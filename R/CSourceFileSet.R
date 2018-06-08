#' CSourceFileSet
#'
#' \code{CSourceFileSet} Sources a Corpus object from a directory source.
#'
#' Sources a Corpus object from a directory source. Each file yields a single Document
#' object and a single associated Text object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CSourceFileSet class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @examples
#' dir <- "./foo"
#' corpus <- CSource$new(x = dir, name = "Foo")$dir()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceFileSet <- R6::R6Class(
  classname = "CSourceFileSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CSource0,

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Source Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL) {

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c("FileSet"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'source',
                         event = v$msg, level = "Error")
        stop()
      }

      # Instantiate Corpus
      corpus <- Corpus$new(name = name)
      corpusSource <- paste0(class(x)[1], " (", x$getName(), ")")
      corpus$setMeta(key = 'source', value = corpusSource, type = 'f')

      # Get paths for individual documents
      files <- x$getFiles()
      paths <- unlist(lapply(files, function(f) { f$getFilePath() }))

      lapply(paths, function(p) {

        io <- IOFactory$new()$strategy(p)
        content <- io$read(p)

        # Instantiate Document objects
        name <- tools::file_path_sans_ext(basename(p))
        doc <- Document$new(x = content, name = name)
        doc$setMeta(key = 'source', value = p, type = 'f')

        # Add content and File to Corpus
        corpus$addDocument(x = doc)
      })

      event <- paste0("Corpus ", corpus$getName(), " sourced from ", x$getName())
      corpus$message(event = event)

      return(corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceFileSet(self)
    }
  )
)
