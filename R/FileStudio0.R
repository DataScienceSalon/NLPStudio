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
    #                          Copy Files Method                              #
    #-------------------------------------------------------------------------#
    copyFiles = function(origin, destination, overwrite = FALSE) {
      if (tools::file_ext(destination) == "") {
        if (!file.exists(destination)) {
          dir.create(destination, showWarnings = FALSE, recursive = TRUE)
        }
      }
      files <- private$getFilePaths(origin)
      if (length(files) == 0) {
        event <- paste0("File path, ", origin, ", contains no files to copy.")
        private$logR$log(method = 'copyFiles', event = event, level = "Warn")
      } else {
        for (i in 1:length(files)) {
          to <- file.path(destination, basename(files[[i]]))
          if (file.exists(to) & overwrite ==  FALSE) {
            event <- paste0("Unable to copy files to ", path, " as files already ",
                            "exist in that location. Change the path, delete ",
                            "the existing files, or change the 'overwrite' parameter ",
                            "to TRUE. See?", class(self)[1], " for further assistance.")
            private$logR$log(method = 'copyFiles', event = event, level = "Error")
            stop()
          }
          file.copy(from = files[[i]], to = to)
        }
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                          Move Files Method                              #
    #-------------------------------------------------------------------------#
    moveFiles = function(origin, destination, overwrite = FALSE) {
      if (tools::file_ext(destination) == "") {
        if (!file.exists(destination)) {
          dir.create(destination, showWarnings = FALSE, recursive = TRUE)
        }
      }
      files <- private$getFilePaths(origin)
      if (length(files) == 0) {
        event <- paste0("File path, ", origin, ", contains no files to move.")
        private$logR$log(method = 'moveFiles', event = event, level = "Warn")
      } else {
        for (i in 1:length(files)) {
          to <- file.path(destination, basename(files[[i]]))
          if (file.exists(to) & overwrite ==  FALSE) {
            event <- paste0("Unable to move files to ", path, " as files already ",
                            "exist in that location. Change the path, delete ",
                            "the existing files, or change the 'overwrite' parameter ",
                            "to TRUE. See?", class(self)[1], " for further assistance.")
            private$logR$log(method = 'moveFiles', event = event, level = "Error")
            stop()
          }
          file.rename(from = files[[i]], to = to)
        }
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Create File Set                               #
    #-------------------------------------------------------------------------#
    createFileSet = function(destination, name = NULL) {

      fileSet <- FileSet$new(name)
      fileSet$setMeta(key = 'path', value = destination, type = 'f')
      files <- private$getFilePaths(destination)
      for (i in 1:length(files)) {
        file <- File$new(files[[i]])
        fileSet$addFile(file)
      }
      return(fileSet)
    },

    #-------------------------------------------------------------------------#
    #                         Validate FileSet                                #
    #-------------------------------------------------------------------------#
    validateFileSet = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('FileSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                         Validate FileSource                             #
    #-------------------------------------------------------------------------#

    validateFileSource = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('FileSourceTXT', 'FileSourceCSV',
                                               'FileSourceXML', 'FileSourceURL',
                                               'FileSourceJSON'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
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
