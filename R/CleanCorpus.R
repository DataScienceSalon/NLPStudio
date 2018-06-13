#------------------------------------------------------------------------------#
#                               CleanCorpus                                    #
#------------------------------------------------------------------------------#
#' CleanCorpus
#'
#' \code{CleanCorpus} Encapsulates the command to clean a Corpus or CVSet object.
#'
#' @param x Corpus or CVSet Object
#' @param corpusStudio CorpusStudio object
#' @param name Character string containing the name to assign to the Corpus
#' or CVSet object.
#' @param textConfig TextConfig object containing the instructions for cleaning
#' the object.
#'
#' @return Token object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
CleanCorpus <- R6::R6Class(
  classname = "CleanCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..textConfig = character(),
    ..corpusStudio = character(),
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
    initialize = function(corpusStudio, textConfig, name = NULL) {

      private$loadServices(name = 'CleanCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio', 'textConfig')
      private$..params$classes$objects <- list(corpusStudio, textConfig)
      private$..params$classes$valid <- list(c('CorpusStudio'), c('TextConfig'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..textConfig <- textConfig
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return('Corpus'),
    getOutputClass = function() return('Token'),

    execute = function(x) {

      if (class(x)[1] == 'CVSet') {

        cvSet <- Clone$new()$this(x, reference = TRUE)
        corpora <- x$getCorpus()
        for (i in 1:length(corpora)) {
          private$validateCorpus(param = corpora[[i]], paramName = 'corpus', methodName = 'execute')
          corpus <- private$..corpusStudio$clean(corpora[[i]], private$..textConfig, private$..name)
          cvSet$addCorpus(corpus)
        }
        return(cvSet)
      } else {
        private$validateCorpus(param = corpora[[i]], paramName = 'corpus', methodName = 'execute')
        corpus <- private$..corpusStudio$clean(corpora[[i]], private$..textConfig, private$..name)
        return(corpus)
      }
    }
  )
)
