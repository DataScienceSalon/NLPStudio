## ---- IOCSV
#==============================================================================#
#                                      IOCSV                                   #
#==============================================================================#
#' IOCSV
#'
#'
#' \code{IOCSV} Class responsible for reading and writing csv files.
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
IOCSV <- R6::R6Class(
  classname = "IOCSV",
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

    read = function(path, safe = FALSE, header = TRUE) {

      fileName <- basename(path)

      if (file.exists(path)) {
        text <- read.csv(file = path, header = header,
                            stringsAsFactors = FALSE,
                            sep = ",", quote = "\"'")
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

      write.csv(content, file = path, row.names = FALSE)

      event <- paste0("Successfully wrote ", fileName, ".")
      private$logR$log(method = 'write', event = event)

      invisible(self)
    }
  )
)
