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
        content <- readBin(path, raw(), file.info(path)$size)
        event <- paste0("Successfully read ", fileName, ".")
        private$logR$log( event = event)
      } else {
        event <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
        private$logR$log( event = event, level = "Error")
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

      writeBin(content, path)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log( event = event)

      invisible(self)
    }
  )
)
