#------------------------------------------------------------------------------#
#                               ReadCorpus                                     #
#------------------------------------------------------------------------------#
#' ReadCorpus
#'
#' \code{ReadCorpus} Encapsulates the command to read a Corpus or its texts.
#'
#' @param x Corpus or CVSet Object
#' @param corpusStudio CorpusStudio object
#' @param path Character string the directory or file path
#' the object.
#'
#' @return Token object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
ReadCorpus <- R6::R6Class(
  classname = "ReadCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..corpusStudio = character(),
    ..path = character(),

    #-------------------------------------------------------------------------#
    #                         Validate Corpus                                 #
    #-------------------------------------------------------------------------#
    validateCorpus = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    initialize = function(corpusStudio, path, overwrite = FALSE) {

      private$loadServices(name = 'ReadCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio', 'path')
      private$..params$classes$objects <- list(corpusStudio, path)
      private$..params$classes$valid <- list(c('CorpusStudio'), c('character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..path <- path
      private$..overwrite <- overwrite

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return('Corpus'),
    getOutputClass = function() return('Corpus'),

    execute = function(x) {

      corpus <- private$..corpusStudio$read(x,
                                            path = private$..path,
                                            overwrite = private$..overwrite)
      return(corpus)
    }
  )
)
