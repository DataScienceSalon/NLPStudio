#------------------------------------------------------------------------------#
#                               BuildFileSetCmd                                #
#------------------------------------------------------------------------------#
#' BuildFileSetCmd
#'
#' \code{BuildFileSetCmd} Encapsulates the command to execute to build a FileSet object.
#'
#' @param x Receiver object, e.g. the FileStudio object.
#' @param path Character string containing the relative file or directory path
#' for the file(s).
#' @param name Character string containing the name of the FileSet object.
#' @param target Not used in this command.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
BuildFileSetCmd <- R6::R6Class(
  classname = "BuildFileSetCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..path = character(),
    ..name = character()
  ),

  public = list(
    initialize = function(x, path, name = NULL) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('FileStudio'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      if (!isDirectory(path) & !isFile(path)) {
        event <- "Invalid path file or directory."
        private$logR$log(method = 'initialize', event = event)
        stop()
      }
      private$..x <- x
      private$..path <- path
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..x,

    execute = function(target = NULL) {

      fileSet <- private$..x$build(private$..path, private$..name)
      return(fileSet)
    }
  )
)
