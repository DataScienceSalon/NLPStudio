#------------------------------------------------------------------------------#
#                             FileSourceTxt                                    #
#------------------------------------------------------------------------------#
#' FileSourceTxt
#'
#' \code{FileSourceTxt} Class creates a File object from .txt file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceTxt <- R6::R6Class(
  classname = "FileSourceTxt",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadServices()
      private$validatePath(param = x, paramName = 'x', methodName = 'initialize')

      if (!file.exists(x)) {
        event <- paste0("File ", x, " does not exist.")
        private$logR$log(method = 'initialize', event = event, level = "Error")
        stop()
      }
      private$..x <- x
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Source Method                                 #
    #-------------------------------------------------------------------------#
    source = function() {
      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
