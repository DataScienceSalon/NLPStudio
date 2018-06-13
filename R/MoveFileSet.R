#------------------------------------------------------------------------------#
#                               MoveFileSet                                    #
#------------------------------------------------------------------------------#
#' MoveFileSet
#'
#' \code{MoveFileSet} Encapsulates the command to move a FileSet object.
#'
#' @param to The directory into which the FileSet shall be moved.
#' @param fileStudio FileStudio object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
MoveFileSet <- R6::R6Class(
  classname = "MoveFileSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..to = character(),
    ..fileStudio = character(),
    ..name = character()
  ),

  public = list(
    initialize = function(fileStudio, to, name = NULL) {

      private$loadServices(name = 'MoveFileSet')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('fileStudio')
      private$..params$classes$objects <- list(fileStudio)
      private$..params$classes$valid <- list('FileStudio')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..to <- to
      private$..fileStudio <- fileStudio
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..fileStudio,
    getInputClass = function() return('FileSet'),
    getOutputClass = function() return('FileSet'),

    execute = function(x) {

      fileSet <- private$..fileStudio$move(x, to = private$..to)
      return(fileSet)
    }
  )
)
