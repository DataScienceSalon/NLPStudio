#' File
#'
#' \code{File} Class containinig a file's attributes, content and content management methods.
#'
#' Class contains file attributes, its content as well as basic read, write, encoding
#' and repair methods.
#'
#' @usage finance <- File$new(path = "./finance.txt", name = 'Financial Report')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(path, name = NULL)}}{Initializes an object of the File class.}
#'   \item{\code{read()}}{Reads the file into memory.}
#'   \item{\code{readBin()}}{Reads the file in raw binary format into memory.}
#'   \item{\code{write(x)}}{Writes content to the file.}
#'   \item{\code{writeBin(x)}}{Writes content to the file in raw binary format.}
#'   \item{\code{ctrl(x)}}{Removes nonprintable control characters from the file at the
#'   binary level.}
#'  }
#'
#' @param name Character string containing the name for the File object.
#' @param path Character string containing the file path.
#' @param x Variable containing the file content.
#' @param overwrite Logical. If yes, the write, move, and copy methods
#' overwrite the file in the location designated by the path variable.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Classes
#' @export
File <- R6::R6Class(
  classname = "File",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    #-------------------------------------------------------------------------#
    #                            Constructor                                  #
    #-------------------------------------------------------------------------#
    initialize = function(path, name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$meta$set(key = 'path', value = path, type = 'functional')
      private$meta$set(key = 'name', value = name, type = 'descriptive')

      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Getter Methods                                #
    #-------------------------------------------------------------------------#
    getName = function() {private$meta$get(key = 'name')},
    getFileName = function() {private$meta$get(key = 'fileName')},
    getDirectory = function() {private$meta$get(key = 'directory')},
    getPath = function() {private$meta$get(key = 'path')},


    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {
      path <- private$meta$get(key = 'path')
      io <- IOFactory$new()$strategy(path = path)
      content <- io$read(path = path)
      return(content)
    },

    write = function(x) {
      path <- private$meta$get(key = 'path')
      io <- IOFactory$new()$strategy(path = path)
      io$write(path = path, content = x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Move/Copy                                  #
    #-------------------------------------------------------------------------#
    move = function(path, overwrite = FALSE) {

      # Validation
      if (file.exists(path) & overwrite == FALSE) {
        event <- "File already exists and overwrite is FALSE."
        private$LogR$log(method = 'move', event = event, level = 'error')
        stop()
      }

      # Move file
      if(!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      file.rename(private$..path, path)

      # Log
      event <- paste0("File, ", basename(private$..path), ", moved from ",
                      private$..path, " to ", path, ".")
      private$logR$log(method = 'move', event = event)

      private$meta$set(key = 'path', value = path, type = 'functional')
      invisible(self)
    },

    copy = function(path, overwrite = FALSE) {

      # Validation
      if (file.exists(path) & overwrite == FALSE) {
        event <- "File already exists and overwrite is FALSE."
        private$LogR$log(method = 'move', event = event, level = 'error')
        stop()
      }

      # Copy file
      if(!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      file.copy(private$..path, path)

      # Log
      event <- paste0("File, ", basename(private$..path), ", copied from ",
                      private$..path, " to ", path, ".")
      private$logR$log(method = 'copy', event = event)

      invisible(self)
    },

    ctrl = function(codes = NULL) {
      path <- private$meta$get(key = 'path')
      studio <- FileStudio$new()
      studio$ctrl(path = path, codes = codes)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$File(self)
    }
  )
)
