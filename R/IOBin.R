#==============================================================================#
#                                      IOBin                                   #
#==============================================================================#
#' IOBin
#'
#'
#' \code{IOBin} Class responsible for reading and writing binary files.
#'
#' @template ioStrategyMethods
#'
#' @template ioStrategyParams
#'
#' @template ioStrategyReturn
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOBin <- R6::R6Class(
  classname = "IOBin",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },

    read = function(path, safe = FALSE) {

      fileName <- basename(path)

      if (file.exists(path)) {
        text <- readBin(path, raw(), file.info(path)$size)
        event <- paste0("Successfully read ", fileName, ".")
        private$logR$log( event = event)
      } else {
        event <- paste0('Unable to read ', fileName, '. ',
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

      writeBin(content, path)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(method = 'write', event = event)

      invisible(self)
    }
  )
)
