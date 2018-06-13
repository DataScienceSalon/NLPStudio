#------------------------------------------------------------------------------#
#                               SplitCorpus                                    #
#------------------------------------------------------------------------------#
#' SplitCorpus
#'
#' \code{SplitCorpus} Encapsulates the command to split a Corpus object.
#'
#' Encapsulates the command to split a Corpus object into a cross-validation
#' object (CV), containing a training, test, and optional validation sets.
#'
#' @param x Corpus or Token Object
#' @param corpusStudio CorpusStudio object
#' @param name Character string containing the name to assign to the cross
#' validation (CVSet) object.
#' @param train Numeric indicating the proportion of the Corpus to allocate
#' to the training set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param validation Numeric indicating the proportion of the Corpus to allocate
#' to the validation set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param test Numeric indicating the proportion of the Corpus to allocate
#' to the test set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @return Token object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
SplitCorpus <- R6::R6Class(
  classname = "SplitCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..name = character(),
    ..corpusStudio = character(),
    ..train = numeric(),
    ..validation = numeric(),
    ..test = numeric(),
    ..stratify = logical(),
    ..seed = character(),

    #-------------------------------------------------------------------------#
    #                         Validate Corpus                                 #
    #-------------------------------------------------------------------------#
    validateCorpus = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('Corpus', 'Token'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    initialize = function(corpusStudio, name = NULL, train = 0.75, validation = 0,
                          test = 0.25,  stratify = TRUE, seed = NULL) {

      private$loadServices(name = 'SplitCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio', 'train',
                                            'validation', 'test')
      private$..params$classes$objects <- list(corpusStudio, train,
                                               validation, test)
      private$..params$classes$valid <- list(c('CorpusStudio'),
                                             c('numeric'),
                                             c('numeric'),
                                             c('numeric'))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..name <- name
      private$..train <- train
      private$..validation <- validation
      private$..test <- test
      private$..stratify <- stratify
      private$..seed <- seed

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return(c('Corpus', 'Token')),
    getOutputClass = function() return('CVSet'),

    execute = function(x) {

      private$validateCorpus(param = x, paramName = 'x', methodName = 'execute')
      cvSet <- private$..corpusStudio$split(x, name = private$..name,
                                            train = private$..train,
                                            validation = private$..validation,
                                            test = private$..test,
                                            stratify = private$..stratify,
                                            seed = private$..seed)
      return(cvSet)
    }
  )
)
