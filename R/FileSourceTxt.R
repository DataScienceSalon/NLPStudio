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
  inherit = FileSource0,

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    source = function(path) {

      # Validation
      if (!file.exists(path)) {
        event = paste0("Invalid path, ", path, " does not exist.")
        private$logR$log(method = 'build', event = event, level = "Error")
        stop()
      }

      file <- File$new(path = path)
      return(file)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
