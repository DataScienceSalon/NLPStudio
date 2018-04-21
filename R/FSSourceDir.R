#' FSSourceDir
#'
#' \code{FSSourceDir} Sources a FileSet object from a directory source.
#'
#' Sources a FileSet object from a directory source. Each file yields a single Document
#' object and a single associated Text object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the FSSourceDir class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the FileSet object.}
#'  }
#'
#' @param name Optional character vector indicating name for FileSet object.
#' @param x Character string containing the directory from which the FileSet
#' object will be source.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Set Source Classes
#' @export
FSSourceDir <- R6::R6Class(
  classname = "FSSourceDir",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FSSource0,

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

      private$..fileSet <- FileSet$new(path = x, name = name)
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function() {

      files <- NLPStudio::listFiles(private$..x)

      lapply(files, function(f) {

        # Instantiate File and Document objects
        if (private$..docNames) {
          name <- tools::file_path_sans_ext(basename(f))
          file <- File$new(path = f, name = name)
        } else {
          file <- File$new(path = f)
        }

        # Add content and File to FileSet
        private$..fileSet$addFile(x = file)
      })

      return(private$..fileSet)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fcsourceDir(self)
    }
  )
)
