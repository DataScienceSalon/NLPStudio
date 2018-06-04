#------------------------------------------------------------------------------#
#                                    File                                      #
#------------------------------------------------------------------------------#
#' File
#'
#' \code{File} Class respresenting an individual file.
#'
#' @param path Character string containing a relative path to a file.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family File Classes
#' @export
File <- R6::R6Class(
  classname = "File",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Primitive0,

  public = list(
    initialize = function(path) {
      name <- tools::file_path_sans_ext(basename(path))
      private$loadServices(name = name)
      if (!file.exists(path)) {
        event <- paste0("File ", path, " does not exist.")
        private$logR$log(method = 'description', event = event, level = "Error")
        stop()
      }
      private$meta$set(key = 'path', value = path, type = 'f')
      private$meta$set(key = 'fileName', value = basename(path), type = 'f')
      invisible(self)
    },

    getFileName = function() private$meta$get(key = 'fileName'),
    getFilePath = function() private$meta$get(key = 'path'),
    getQuant = function() private$meta$get(type = 'q'),
    setQuant = function(quant) {
      private$meta$set(key = names(quant), value = unlist(quant), type = 'q')
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$file(self)
    }
  )
)
