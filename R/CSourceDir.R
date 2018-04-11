#' CSourceDir
#'
#' \code{CSourceDir} Sources a Corpus object from a directory source.
#'
#' Sources a Corpus object from a directory source. Each file yields a single Document
#' object and a single associated Text object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CSourceDir class.}
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
CSourceDir <- R6::R6Class(
  classname = "CSourceDir",
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
    source = function(x, name = NULL, encoding = NULL, ctrl = TRUE,
                      docNames = TRUE) {

      private$..corpus$setMeta(key = 'name', value = name)

      if (isDirectory(x)) {
        paths <- list.files(x, full.names = TRUE)
      } else {
        glob <- basename(x)
        dir <- dirname(x)
        paths <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      lapply(paths, function(p) {

        # Obtain file object
        if (docNames) {
          file <- File$new(path = p,
                           name = tools::file_path_sans_ext(basename(f)))
        } else {
          file <- File$new(path = p)
        }

        # Replace control characters
        if (ctrl) {
          studio <- FileStudio$new()
          studio$ctrl(path = p)
        }

        # Read content and create Document objects.
        name <- file$getName()
        content <- IOText$new()$read(p)
        doc <- Document$new(x = content, name = name)
        private$..corpus$addDocument(x = doc)
      })

      return(private$..corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceDir(self)
    }
  )
)
