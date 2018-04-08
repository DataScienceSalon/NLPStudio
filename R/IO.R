#' IO
#'
#' \code{IO} Class responsible for Text IO within the NLPStudio package.
#'
#' Class responsible for reading and writing texts into and from the NLPStudio
#' package. Read methods support a range of formats and has a repair option
#' that converts select ASCII control characters to spaces by default.
#'
#' @usage myText <- IO$new(name = "skiReport", x = avalanche)
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{read(path)}}{Reads text. If the file is a ".txt" file, the repair
#'   process replaces select ASCII control characters with spaces.}
#'   \item{\code{write(path, content)}}{Write text to designated file location.}
#'  }
#'
#' @param path Character string containing the file path
#' @param content Character vectors containing the content to be written
#'
#' @return Read method returns character vectors containing text. The write method
#' returns an instance of the class.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family IO Classes
#' @export
IO <- R6::R6Class(
  classname = "IO",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    logR = character()
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                          Initialize Method                              #
    #-------------------------------------------------------------------------#

    initialize = function() {
      private$logR <- LogR$new()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Read Method                                 #
    #-------------------------------------------------------------------------#
    read = function(path, repair = FALSE) {

      private$..methodName <- 'read'

      # Validate path
      if (!R.utils::isFile(path)) {
        event <- paste0("File path ", path, " not found.")
        private$logR$log( event = event, level = "Error")
        stop()
      } else if (is.null(IOFactory$new(path)$getIOStrategy())) {
        event <- paste0("File ", path, " is an unsupported file type.")
        private$logR$log( event = event, level = "Error")
        stop()
      }

      if (repair & tools::file_ext(path) == "txt") {
        content <- RepairFile$new(path)$execute()
      } else {
        io <- IOFactory$new(path)$getIOStrategy()
        content <- io$read(path = path)
      }

      # Update log and system metadata
      event <- paste0("Read file from ", path, ". ")
      private$logR$log( event = event)

      return(content)
    },

    #-------------------------------------------------------------------------#
    #                            Write Method                                 #
    #-------------------------------------------------------------------------#
    write = function(path, content) {

      private$..methodName <- 'write'

      io <- IOFactory$new(path)$getIOStrategy()
      io$write(path = path, content = content)

      # Update log
      event <- paste0("Saved content to ", path, ". ")
      private$logR$log( event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$io(self)
    }
  )
)
