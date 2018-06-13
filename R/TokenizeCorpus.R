#------------------------------------------------------------------------------#
#                             TokenizeCorpus                                   #
#------------------------------------------------------------------------------#
#' TokenizeCorpus
#'
#' \code{TokenizeCorpus} Encapsulates the command to tokenize a Corpus object.
#'
#' @param x Corpus Object
#' @param corpusStudio CorpusStudio object
#' @param name Character string containing the name to assign to the Token object.
#' @param tokenUnit Character string indicating the type of Token object to
#' produce. Valid values are c('character', 'word', 'sentence', 'paragraph')
#'
#' @return Token object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
TokenizeCorpus <- R6::R6Class(
  classname = "TokenizeCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..corpusStudio = character(),
    ..tokenUnit = character(),
    ..name = character(),

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
    initialize = function(corpusStudio, tokenUnit, name = NULL) {

      private$loadServices(name = 'TokenizeCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio')
      private$..params$classes$objects <- list(corpusStudio)
      private$..params$classes$valid <- list(c('CorpusStudio'))
      private$..params$discrete$variables = list('tokenUnit')
      private$..params$discrete$values = list(tokenUnit)
      private$..params$discrete$valid = list(c('character', 'word', 'sentence', 'paragraph'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..tokenUnit <- tokenUnit
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return('Corpus'),
    getOutputClass = function() return('Token'),

    execute = function(x) {

      private$validateCorpus(param = x, paramName = 'x', methodName = 'execute')
      token <- private$..corpusStudio$tokenize(x, tokenUnit = private$..tokenUnit,
                                                name = private$..name)
      return(token)
    }
  )
)
