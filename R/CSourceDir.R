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
    source = function(x, name = NULL, encoding = NULL, replaceCtrl = FALSE,
                      docNames = TRUE, codes = NULL) {

      # Validation
      if (!is.null(encoding)) {
        private$..params <- list()
        private$..params$discrete$variables <- list("encoding")
        private$..params$discrete$values <- list(encoding)
        private$..params$discrete$valid <- list(c("UTF-8", "latin1", "bytes"))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'source',
                           event = v$msg, level = "Error")
          stop()
        }
      }

      if (!is.null(codes)) {
        if (!is.numeric(codes)) stop(paste("Invalid codes parameter. Numeric vector,",
                                           "of numbers between 0 and 31 expected."))
      }

      if (!is.null(name)) {
        private$..corpus$setMeta(key = 'name', value = name)
      }

      if (isDirectory(x)) {
        paths <- list.files(x, full.names = TRUE)
      } else {
        glob <- basename(x)
        dir <- dirname(x)
        paths <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      lapply(paths, function(p) {

        # Instantiate File and Document objects
        if (docNames) {
          name <- tools::file_path_sans_ext(basename(p))
          file <- File$new(path = p, name = name)
          doc <- Document$new(name = name)
        } else {
          file <- File$new(path = p)
          doc <- Document$new()
        }

        # Conduct required file conditioning
        studio <- FileStudio$new()
        if (replaceCtrl) {
          studio$ctrl(file, codes)
          file$message("Removed control characters from text.")
        }
        if (!is.null(encoding)) {
          studio$encode(file, encoding)
          file$message("Declared and converted encoding to 'UTF-8'.")
        }


        # Add content and File to Corpus
        private$..corpus$addDocument(x = file)
        name <- file$getName()
        content <- file$read()
        doc$text(content)
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
