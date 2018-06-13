#------------------------------------------------------------------------------#
#                               RepairFileSet                                  #
#------------------------------------------------------------------------------#
#' RepairFileSet
#'
#' \code{RepairFileSet} Encapsulates the command to execute to repair a FileSet object.
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
RepairFileSet <- R6::R6Class(
  classname = "RepairFileSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..fileStudio = character(),
    ..codes = character()
  ),

  public = list(
    initialize = function(fileStudio, codes = NLPStudio:::nonPrintables) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('fileStudio')
      private$..params$classes$objects <- list(fileStudio)
      private$..params$classes$valid <- list(c('FileStudio'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'execute', event = v$msg, level = 'Error')
        stop()
      }

      private$..fileStudio <- fileStudio
      private$..codes <- codes

      invisible(self)
    },

    getReceiver = function() private$..fileStudio,

    execute = function(x) {

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('FileSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'execute', event = v$msg, level = 'Error')
        stop()
      }


      fileSet <- private$..fileStudio$repair(x = x, codes = private$..codes)
      return(fileSet)
    }
  )
)
