#------------------------------------------------------------------------------#
#                               SampleCorpus                                   #
#------------------------------------------------------------------------------#
#' SampleCorpus
#'
#' \code{SampleCorpus} Encapsulates the command to sample a Corpus object.
#'
#' @param x Corpus or Token Object
#' @param corpusStudio CorpusStudio object
#' @param name Character string containing the name to assign to the Token object.
#' @param n Numeric. If n <= 1, the proportion of 'unit' to sample. If n>1, the number
#' of 'unit' to sample.
#' @param stratify Logical. Applies to Corpus objects with with two or more documents.
#' If TRUE, stratified sampling will occur within each document; otherwise, samples
#' will be taken from a single combined document. Default is TRUE
#' @param replace Logical. If TRUE, samples will be taken with replacement. Default is FALSE.
#' @param seed Numeric. Seed for sampling reproducibility. Default is NULL
#'
#' @return Token object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
SampleCorpus <- R6::R6Class(
  classname = "SampleCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..x = character(),
    ..corpusStudio = character(),
    ..n = numeric(),
    ..stratify = logical(),
    ..replace = logical(),
    ..name = character(),
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
    initialize = function(corpusStudio, n, name = NULL, stratify = TRUE,
                          replace = FALSE, seed = NULL) {

      private$loadServices(name = 'SampleCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio')
      private$..params$classes$objects <- list(corpusStudio)
      private$..params$classes$valid <- list(c('CorpusStudio'))
      private$..params$logicals$variables <- c('stratify', 'replace')
      private$..params$logicals$values <- c(stratify, replace)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..n <- n
      private$..stratify <- stratify
      private$..replace <- replace
      private$..seed <- seed
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return('Corpus'),
    getOutputClass = function() return('Token'),

    execute = function(x) {

      private$validateCorpus(param = x, paramName = 'x', methodName = 'execute')
      token <- private$..corpusStudio$sample(x, n = private$..n,
                                             name = private$..name,
                                             stratify = private$..stratify,
                                             replace = private$..replace,
                                             seed = private$..seed)
      return(token)
    }
  )
)
