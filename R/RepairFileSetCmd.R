#------------------------------------------------------------------------------#
#                               RepairFileSetCmd                                #
#------------------------------------------------------------------------------#
#' RepairFileSetCmd
#'
#' \code{RepairFileSetCmd} Encapsulates the command to execute to repair a FileSet object.
#'
#' @param x Receiver object, e.g. the FileStudio object.
#' @param path Character string containing the relative file or directory path
#' to which the repaired file will be stored.
#' @param target FileSet object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
RepairFileSetCmd <- R6::R6Class(
  classname = "RepairFileSetCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..path = character()
  ),

  public = list(
    initialize = function(x, path, codes = NLPStudio:::nonPrintables) {

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

      private$..x <- x
      private$..path <- path

      invisible(self)
    },

    getReceiver = function() private$..x,

    execute = function(target = NULL) {

      fileSet <- private$..x$repair(fileSet = target, newPath = private$..path)
      return(fileSet)
    }
  )
)
