#------------------------------------------------------------------------------#
#                             FileSourcePDF                                    #
#------------------------------------------------------------------------------#
#' FileSourcePDF
#'
#' \code{FileSourcePDF} Class creates a File object from .txt file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourcePDF <- R6::R6Class(
  classname = "FileSourcePDF",
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

      private$loadServices(name = 'FileSourcePDF')
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
