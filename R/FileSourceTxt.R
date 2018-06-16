#------------------------------------------------------------------------------#
#                             FileSourceTXT                                    #
#------------------------------------------------------------------------------#
#' FileSourceTXT
#'
#' \code{FileSourceTXT} Class creates a File object from .txt file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceTXT <- R6::R6Class(
  classname = "FileSourceTXT",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileSource0,

  private = list(
    #-------------------------------------------------------------------------#
    #                          Source Files Method                            #
    #-------------------------------------------------------------------------#
    sourceFiles = function(origin, destination, overwrite) {
      private$copyFiles(origin, destination, overwrite)
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadServices(name = 'FileSourceTXT')
      private$..origin <- x
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            BuildFileSet                                 #
    #-------------------------------------------------------------------------#
    buildFileSet = function(destination, name = NULL, overwrite = FALSE) {

      private$sourceFiles(origin = private$..origin, destination = destination,
                          overwrite= overwrite)
      fileSet <- private$createFileSet(path = destination, name = name)

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
