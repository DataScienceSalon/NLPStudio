#------------------------------------------------------------------------------#
#                                    Repair                                    #
#------------------------------------------------------------------------------#
#' Repair
#'
#' \code{Repair}  Class responsible for performing repairs on files.
#'
#' Class replaces select control characters with spaces at the binary level.
#'
#' @param input Character string containing the relative file or directory path
#' for the file(s) to be repaired.
#' @param output Character string containing the relative file or directory path
#' to which the repaired file is stored.
#' @param codes The ASCII codes for non printable characters to be replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @export
Repair <- R6::R6Class(
  classname = "Repair",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    processFile = function(input, output, codes = NLPStudio:::nonPrintables) {

      ioBin <- IOBin$new()
      ioTxt <- IOText$new()

      # Read and repair content
      content <- ioBin$read(path = input)
      for (i in 1:length(codes)) {
        content[content == as.raw(codes[i])] = as.raw(0x20)
      }

      # Save to temp file, then re-read
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- ioTxt$read(path = d)
      unlink(d)

      # Save as text file
      if (!dir.exists(dirname(output))) dir.create(dirname(output))
      ioTxt$write(path = output, content = content)

      return(TRUE)
    }
  ),

  public = list(

    initialize = function() {

      private$loadServices()
      invisible(self)
    },

    execute = function(input, output, codes = NLPStudio:::nonPrintables) {

      if (isDirectory(input)) {
        input <- list.files(input, full.names = TRUE)
      } else {
        glob <- basename(input)
        dir <- dirname(input)
        input <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      # Process files
      for (i in 1:length(input)) {
        private$processFile(input[i],
                            file.path(output, basename(input[i])), codes)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ", input, ". ")
      private$logR$log(method = 'execute', event = event)

      return(output)
    }
  )
)
