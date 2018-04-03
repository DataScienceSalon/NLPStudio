## ---- IORdata
#==============================================================================#
#                                 IORdata                                      #
#==============================================================================#
#' IORdata
#'
#'
#' \code{IORdata} Class responsible for reading and writing Rdata files.
#'
#' @usage IORdata$new()$read(path)
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
IORdata <- R6::R6Class(
  classname = "IORdata",
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
        env <- new.env()
        content <- load(path, envir = env)
        content <- env[[content]]
        event <- paste0("Successfully read ", fileName, ".")
        private$logR$log(cls = class(self)[1], event = event)
      } else {
        event <- paste0('Unable to read ', fileName, '. ',
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

      save(object = content, file = path, compression_level = 9)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)
    }
  )
)
