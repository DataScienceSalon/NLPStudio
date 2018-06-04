#------------------------------------------------------------------------------#
#                                    FileSet                                   #
#------------------------------------------------------------------------------#
#' FileSet
#'
#' \code{FileSet} Class respresenting a set or collection of File objects.
#'
#' @param file File object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family File Classes
#' @export
FileSet <- R6::R6Class(
  classname = "FileSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Primitive0,

  private = list(
    validate = function(file, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list('file')
      private$..params$classes$objects <- list(file)
      private$..params$classes$valid <- list(c('File'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
    }
  ),

  public = list(
    initialize = function(name = NULL) {
      loadServices(name = name)
    },

    #-------------------------------------------------------------------------#
    #                           Composite Methods                             #
    #-------------------------------------------------------------------------#
    getFiles = function() {
      private$..children
    },

    addFile = function(file)  {
      private$validate(file, methodName = 'addFile')
      private$attach(file)
      invisible(self)
    },

    removeFile = function(file) {
      private$validate(file, methodName = 'removeFile')
      private$detach(file)
      invisible(self)
    },

    purgeFiles = function() {
      private$..children <- NULL
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSet(self)
    }
  )
)
