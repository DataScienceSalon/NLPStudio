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
#'   \item{\code{write(path, content)}}{Write method.}
#' }
#'
#' @param path Character string containing the relative file path
#' @param content Character vector to be written to file
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

  private = list(
    logR = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path) {

      private$logR <- LogR$new()

      fileName <- basename(path)

      if (file.exists(path)) {
        con <- file(path)
        on.exit(close(con))
        content <- readLines(con)
        event <- paste0("Successfully read ", fileName, ".")
        private$logR$log(cls = class(self)[1], event = event)
      } else {
        event <- paste0('Unable to read ', path, '. ',
                                  'File does not exist.')
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      }
      return(content)
    },

    write = function(path, content) {

      private$logR <- LogR$new()

      fileName <- basename(path)
      dirName <- dirname(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      con <- file(path)
      on.exit(close(con))
      writeLines(content, con)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    }
  )
)
