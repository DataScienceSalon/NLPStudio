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


  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },

    read = function(path) {

      fileName <- basename(path)

      if (file.exists(path)) {
        con <- file(path)
        on.exit(close(con))
        text <- readLines(con)
        event <- paste0("Successfully read ", fileName, ".")
        private$logR$log( event = event)
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
