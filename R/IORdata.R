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
        env <- new.env()
        text <- load(path, envir = env)
        text <- env[[text]]
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

      save(object = content, file = path, compression_level = 9)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(method = 'write', event = event)

      invisible(self)
    }
  )
)
