#==============================================================================#
#                               FileStudio                                     #
#==============================================================================#
#' FileStudio
#'
#' \code{FileStudio} Class for treating and repair files.
#'
#' Class responsible for treating and performing repairs on text files
#' at binary level.
#'
#' @param path Character string indicating the path to a file to be repaired.
#' @param code Numeric vector containing up to 32 numbers between 0 and 31,
#' in which each number relates to the decimal ASCII code for the control
#' characters to remove from the file.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Classes
#' @export
FileStudio <- R6::R6Class(
  classname = "FileStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Replace Control Characters                        #
    #-------------------------------------------------------------------------#
    ctrl = function(file, codes = NULL) {

      path <- file$getMeta(key = 'path')

      # Validate path and file type
      if (!R.utils::isFile(path)) stop(paste("File", path, "does not exist."))
      if (tools::file_ext(path) != "txt") {
        stop("This method operates on '.txt' files only.")
      }

      ioBin <- IOBin$new()
      ioTxt <- IOText$new()

      content <- ioBin$read(path = path)

      if (is.null(codes)) codes <- seq(0,31)

      for (i in 1:length(codes)) {
        content[content == as.raw(codes[i])] = as.raw(32)
      }

      # Save to temp file and re-read
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- ioTxt$read(path = d)

      # Write back to original location
      ioTxt$write(path = path, content = content)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                Encoding                                 #
    #-------------------------------------------------------------------------#
    encode = function(file, encoding = 'latin1') {

      private$..params <- list()
      private$..params$discrete$variables <- list("encoding")
      private$..params$discrete$values <- list(encoding)
      private$..params$discrete$valid <- list(c("UTF-8", "latin1", "bytes"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'encode',
                         event = v$msg, level = "Error")
        stop()
      }

      path <- file$getPath()

      io <- IOFactory$new()$strategy(path)
      content <- io$read(path)
      Encoding(content) <- encoding
      content <- enc2utf8(content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")
      io$write(content = content, path = path)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Visitor Method                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
