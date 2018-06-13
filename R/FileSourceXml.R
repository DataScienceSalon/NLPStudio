#------------------------------------------------------------------------------#
#                             FileSourceXML                                    #
#------------------------------------------------------------------------------#
#' FileSourceXML
#'
#' \code{FileSourceXML} Class creates a File object from .txt file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceXML <- R6::R6Class(
  classname = "FileSourceXML",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(

    copyFiles = function(path) {
      file.copy(from = private$..path, to = path, recursive = TRUE)
      return(TRUE)
    },

    createFileSet = function(path) {

      private$copyFiles(path)

      fileSet <- FileSet$new(private$..name)
      fileSet$setMeta(key = 'path', value = path, type = 'f')
      files <- private$getFilePaths(path)
      for (i in 1:length(files)) {
        file <- File$new(files[[i]])
        fileSet$addFile(file)
      }
      return(fileSet)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL) {

      private$loadServices(name = 'FileSourceXML')
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
