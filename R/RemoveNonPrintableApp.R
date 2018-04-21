#------------------------------------------------------------------------------#
#                             Replace Numbers App                              #
#------------------------------------------------------------------------------#
#' RemoveNonPrintableApp
#'
#' \code{RemoveNonPrintableApp}  Removes non-printable characters from a File or FileCollection
#'
#' @template textStudioParams
#' @param codes Vector of integers representing the decimal code for control
#' characters to be removed from the file.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Classes
#' @export
RemoveNonPrintableApp <- R6::R6Class(
  classname = "RemoveNonPrintableApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(

    ..codes = numeric(),

    processFile = function(file) {

      path <- file$getPath()

      # Validate path and file type
      if (!R.utils::isFile(path)) stop(paste("File", path, "does not exist."))
      if (tools::file_ext(path) != "txt") {
        stop("This method operates on '.txt' files only.")
      }

      ioBin <- IOBin$new()
      ioTxt <- IOText$new()

      content <- ioBin$read(path = path)
      for (i in 1:length(private$..codes)) {
        content[content == as.raw(private$..codes[i])] = as.raw(0x20)
      }

      # Save to temp file, re-read, then rewrite to original file location
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- ioTxt$read(path = d)
      unlink(d)
      Encoding(content) <- "latin1"
      content <- enc2utf8(content)
      content <- gsub("â€™", "'", content)
      content <- gsub("â€˜", "'", content)
      content <- gsub("â€¦", "", content)
      content <- gsub("â€", "-", content)
      content <- iconv(content, "UTF-8", "ASCII", sub = "")
      file$write(content)

      return(file)
    },

    processCollection = function() {
      files <- private$..out$getFiles()
      for (i in 1:length(files)) {
        file <- private$processFile(files[[i]])
        private$..out$addFile(file)
      }
      return(private$..out)
    }
  ),

  public = list(
    initialize = function(x, codes = c(0,26)) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x', 'codes')
      private$..params$classes$objects <- list(x, codes)
      private$..params$classes$valid <- list(c('File', 'FileCollection'), c("numeric", "integer"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..out <- x
      private$..codes <- codes

      invisible(self)
    }
  )
)
