#' FCSourceDir
#'
#' \code{FCSourceDir} Sources a FileCollection object from a directory source.
#'
#' Sources a FileCollection object from a directory source. Each file yields a single Document
#' object and a single associated Text object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the FCSourceDir class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the FileCollection object.}
#'  }
#'
#' @param name Optional character vector indicating name for FileCollection object.
#' @param x Character string containing the directory from which the FileCollection
#' object will be source.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Collection Source Classes
#' @export
FCSourceDir <- R6::R6Class(
  classname = "FCSourceDir",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FCSource0,

  private = list(
    ..x = character(),
    ..name = character(),
    ..docNames = logical()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL,  docNames = TRUE) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$logicals$variables <- list("docNames")
      private$..params$logicals$values <- list(docNames)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'source',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..name <- name
      private$..docNames <- docNames

      private$..fileCollection <- FileCollection$new(path = x, name = name)
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function() {

      if (isDirectory(private$..x)) {
        paths <- list.files(private$..x, full.names = TRUE)
      } else {
        glob <- basename(private$..x)
        dir <- dirname(private$..x)
        paths <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      lapply(paths, function(p) {

        # Instantiate File and Document objects
        if (private$..docNames) {
          name <- tools::file_path_sans_ext(basename(p))
          file <- File$new(path = p, name = name)
        } else {
          file <- File$new(path = p)
        }

        # Add content and File to FileCollection
        private$..fileCollection$addFile(x = file)
      })

      return(private$..fileCollection)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fcsourceDir(self)
    }
  )
)
