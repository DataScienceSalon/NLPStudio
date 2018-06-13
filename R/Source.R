#------------------------------------------------------------------------------#
#                                  Source                                      #
#------------------------------------------------------------------------------#
#' Source
#'
#' \code{Source} Encapsulates the command to source a FileSet object.
#'
#' @param x FileSource Object
#' @param fileStudio FileStudio object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
Source <- R6::R6Class(
  classname = "Source",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..fileStudio = character(),
    ..name = character()
  ),

  public = list(
    initialize = function(fileStudio, name = NULL) {

      private$loadServices(name = 'Source')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('fileStudio')
      private$..params$classes$objects <- list(fileStudio)
      private$..params$classes$valid <- list(c('FileStudio'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..fileStudio <- fileStudio
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..fileStudio,
    getInputClass = function() return(c('FileSourceTXT', 'FileSourceCSV',
                                 'FileSourceXML', 'FileSourceURL',
                                 'FileSourceJSON', 'FileSourceDocx')),
    getOutputClass = function() return('FileSet'),

    execute = function(x) {

      fileSet <- private$..fileStudio$source(x, name = private$..name)
      return(fileSet)
    }
  )
)
