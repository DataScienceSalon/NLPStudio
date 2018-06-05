#------------------------------------------------------------------------------#
#                                    FileSource0                               #
#------------------------------------------------------------------------------#
#' FileSource0
#'
#' \code{FileSource0}  Abstract class for the FileSource family of classes.
#'
#' Abstract strategy class which defines the methods common to the FileSource
#' family of classes.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSource0 <- R6::R6Class(
  classname = "FileSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    #-------------------------------------------------------------------------#
    #                       Validate Character Method                         #
    #-------------------------------------------------------------------------#
    validateChar = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      } else if (length(param) > 1) {
        event <- paste0("Invalid ", paramName, " parameter.  Must be a single character string.")
        private$logR$log(method = methodName, event = event, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() { stop(paste("This method is not for this abstract",
                                         "class.")) },

    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    source = function(path) { stop(paste("This method is not for this abstract",
                                        "class.")) },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
