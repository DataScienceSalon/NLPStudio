#------------------------------------------------------------------------------#
#                             FileSourceURL                                    #
#------------------------------------------------------------------------------#
#' FileSourceURL
#'
#' \code{FileSourceURL} Class creates a File object from .txt file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceURL <- R6::R6Class(
  classname = "FileSourceURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(
    #-------------------------------------------------------------------------#
    #                       Validate URL Method                               #
    #-------------------------------------------------------------------------#
    validateURL = function(param, paramName, methodName) {

      if (!url.exists(param)) {
        event <- paste0("URL , ", param, ", does not exist.")
        private$logR$log(method = methodName, event = event, level = "Error")
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$loadServices(name = 'FileSourceURL')
      private$..path <- x
      private$..name <- name

      invisible(self)
    },
    buildFileSet = function(path) {
      fileSet <- private$createFileSet(path)
      return(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
