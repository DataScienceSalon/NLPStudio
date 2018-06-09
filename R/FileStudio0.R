#------------------------------------------------------------------------------#
#                                    FileStudio0                               #
#------------------------------------------------------------------------------#
#' FileStudio0
#'
#' \code{FileStudio0}  Abstract class for the FileStudio family of classes.
#'
#' Abstract strategy class which defines the methods common to the FileStudio
#' family of classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Family of Classes
#' @export
FileStudio0 <- R6::R6Class(
  classname = "FileStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    #-------------------------------------------------------------------------#
    #                          Get File Paths Method                          #
    #-------------------------------------------------------------------------#
    getFilePaths = function(path) {
      if (isDirectory(path)) {
        files <- list.files(path, full.names = TRUE)
      } else {
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }
      return(files)
    },

    #-------------------------------------------------------------------------#
    #                         Validate Path Method                            #
    #-------------------------------------------------------------------------#
    validatePath = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      } else if (length(param) > 1) {
        event <- paste0("Invalid ", paramName, " parameter. ",
                        "Must be a single character string.")
        private$logR$log(method = methodName, event = event, level = "Error")
        stop()
      } else if (grepl(" ", param, perl = TRUE)) {
        event <- paste0("Invalid ", paramName, " parameter. ",
                        "Must be a valid file path without spaces.")
        private$logR$log(method = methodName, event = event, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    initialize = function(x) { stop(paste("This method is not for this abstract",
                                         "class.")) },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
