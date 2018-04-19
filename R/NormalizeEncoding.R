#------------------------------------------------------------------------------#
#                              NormalizeEncoding                               #
#------------------------------------------------------------------------------#
#' NormalizeEncoding
#'
#' \code{NormalizeEncoding} Command for the NormalizeEncoding class.
#'
#' Class that encapsulates the command to execute an object of the NormalizeEncodingApp
#' class
#'
#' @usage NormalizeEncoding$new(contractions = NULL, ignoreCase = TRUE)
#'
#' @template textStudioParams
#' @param encoding Character string indicating the type of encoding. Valid
#' values are c('latin1', 'UTF-8', 'bytes')
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
NormalizeEncoding <- R6::R6Class(
  classname = "NormalizeEncoding",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..encoding = character()
  ),

  public = list(
    initialize = function(encoding = "latin1") {

      private$loadDependencies()

      # Validate parameters
      private$..params$discrete$variables <- c('encoding')
      private$..params$discrete$values <- list(encoding)
      private$..params$discrete$valid <-  list(c('latin1', 'UTF-8', 'bytes'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..encoding <- encoding

      invisible(self)
    },
    execute = function(x) {
      x <- NormalizeEncodingApp$new(x, encoding = private$..encoding)$execute()
      return(x)
    }
  )
)
