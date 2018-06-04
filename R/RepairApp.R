#------------------------------------------------------------------------------#
#                                 Repair App                                   #
#------------------------------------------------------------------------------#
#' RepairApp
#'
#' \code{RepairApp} Encapsulates the command to execute an object of the Repair class.
#'
#' @param x Receiver object, e.g. the FileStudio object.
#' @param input Character string containing the relative file or directory path
#' for the file(s) to be repaired.
#' @param output Character string containing the relative file or directory path
#' to which the repaired file is stored.
#' @param codes = ASCII codes for the control characters to be replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @export
RepairApp <- R6::R6Class(
  classname = "RepairApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = App0,

  private = list(
    ..x = character(),
    ..input = character(),
    ..output = character(),
    ..codes = character()
  ),

  public = list(
    initialize = function(x, input, output, codes = NLPStudio:::nonPrintables) {

      private$loadServices()
      private$..meta <- Meta$new(self)

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

      if (!isDirectory(input) & !isFile(input)) {
        event <- "Invalid input file or directory."
        private$logR$log(method = 'initialize', event = event)
        stop()
      }
      private$..x <- x
      private$..input <- input
      private$..output <- output
      private$..codes <- codes

      invisible(self)
    },
    execute = function() {

      private$..x$repair(input = private$..input, output = private$..output,
                         codes = private$..codes)
      return(private$..x)
    }
  )
)
