#==============================================================================#
#                                      IOText                                  #
#==============================================================================#
#' IOText
#'
#' \code{IOText} Class responsible for reading and writing text files.
#'
#' @usage IOText$new()$read(path = path)
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{read(path)}}{Read method.}
#'   \item{\code{write(path, text)}}{Write method.}
#' }
#'
#' @param path Character string containing the relative file path
#' @param text Character vector to be written to file
#'
#' @return Character string if read method is called. The write method
#' returns TRUE if the write was sucessful, FALSE otherwise.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOText <- R6::R6Class(
  classname = "IOText",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    #-------------------------------------------------------------------------#
    #                            Read File                                    #
    #-------------------------------------------------------------------------#
    readFile = function(path) {

      fileName <- basename(path)

      con <- file(path)
      on.exit(close(con))
      text <- readLines(con)
      event <- paste0("Successfully read ", fileName, ".")
      private$logR$log( event = event)

      return(text)
    },

    #-------------------------------------------------------------------------#
    #                             Read Safe                                   #
    #-------------------------------------------------------------------------#
    readSafe = function(path, codes = NLPStudio:::nonPrintables) {

      ioBin <- IOBin$new()

      # Read and FileStudio content
      content <- ioBin$read(path = path)
      for (i in 1:length(codes)) {
        content[content == as.raw(codes[i])] = as.raw(0x20)
      }

      # Save to temp file, then re-read
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- private$readFile(d)
      unlink(d)

      return(content)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },

    read = function(path, safe = FALSE) {

      if (file.exists(path)) {
        if (safe) {
          text <- private$readSafe(path)
        } else {
          text <- private$readFile(path)
        }
      } else {
        event <- paste0('Unable to read ', path, '. ',
                                  'File does not exist.')
        private$logR$log(method = 'read', event = event, level = "Error")
        stop()
      }
      return(text)
    },

    write = function(path, content) {

      fileName <- basename(path)
      dirName <- dirname(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      con <- file(path)
      on.exit(close(con))
      writeLines(content, con)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(method = 'write', event = event)

      invisible(self)
    }
  )
)
