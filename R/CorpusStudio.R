#------------------------------------------------------------------------------#
#                               CorpusStudio                                   #
#------------------------------------------------------------------------------#
#' CorpusStudio
#'
#' \code{CorpusStudio}  Class responsible for building and manipulating Corpus objects.
#'
#' @param x a series of character vectors, each containing the text for a single
#' document, a FileSet object containing .txt files, a character string
#' containing the directory containing .txt files, a quanteda corpus object, or
#' a tm VCorpus, or tm SimpleCorpus object.
#' @param corpus Corpus object
#' @param name Character string containing the name to assign to the Corpus
#' object.
#' @param path Character string containing a relative path to a directory
#' containing text files or a relative file path for a Corpus object. This
#' parameter is used in the load and save methods.
#' @param mode Character string indicating the mode for writing a Corpus object.
#' Valid values are c("text", "binary", "object"). Abbreviations are permitted.
#' @param k Numeric used by the kFold method. Indicates number of folds in
#' which to split the Corpus object.
#' @param n Numeric parameter used by the sample method. It contains the number
#' of samples to obtain or the proportion of the Corpus to sample.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param replace Logical. If TRUE, sampling is conducted with replacement. The
#' default is FALSE.
#' @param overWrite Logical, Used in the read method. If TRUE, any text or binary
#' files read will overwrite existing versions of the same document source.
#' @param train Numeric indicating the proportion of the Corpus to allocate
#' to the training set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param validation Numeric indicating the proportion of the Corpus to allocate
#' to the validation set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param test Numeric indicating the proportion of the Corpus to allocate
#' to the test set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family CorpusStudio Family of Classes
#' @export
CorpusStudio <- R6::R6Class(
  classname = "CorpusStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..corpus = character(),
    ..clean  = character(),
    ..tokens = character(),
    ..sample = character(),
    ..splits = character(),
    ..kFolds = character(),

    validate = function(corpus, methodName) {

      private$..params <- list()
      private$..params$classes$name <- list('corpus')
      private$..params$classes$objects <- list(corpus)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices(name = 'CorpusStudio')
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(x, name = NULL) {

      sourceType <- class(x)[1]
      private$..params <- list()
      private$..params$discrete$variables = list('sourceType')
      private$..params$discrete$values = list(sourceType)
      private$..params$discrete$valid = list(c('character', 'FileSet', 'corpus',
                                               'VCorpus', 'SimpleCorpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build', event = v$msg, level = "Error")
        stop()
      }

      if (sourceType == 'character') {
        if (isDirectory(x)) {
          sourceType <- 'directory'
        }
      }

      private$..corpus <- switch(sourceType,
                                 FileSet = CSourceFileSet$new()$source(x, name),
                                 corpus = CSourceQuanteda$new()$source(x, name),
                                 VCorpus = CSourceTM$new()$source(x, name),
                                 SimpleCorpus = CSourceTM$new()$source(x, name),
                                 character = CSourceVector$new()$source(x, name),
                                 directory = CSourceDir$new()$source(x, name))

      event <- paste0("Constructed Corpus from ", sourceType, ".")
      private$..corpus$message(event)
      private$logR$log(method = 'build', event = event, level = "Info")
      invisible(self)
    },

    getCorpus = function() private$..corpus,

    #-------------------------------------------------------------------------#
    #                      Corpus Tokenize Method                             #
    #-------------------------------------------------------------------------#
    tokenize = function(tokenUnit = 'sentence') {

      # Validation
      private$validate(private$..corpus, methodName = 'tokenize')

      private$..params <- list()
      private$..params$discrete$variables = list('tokenUnit')
      private$..params$discrete$values = list(tokenUnit)
      private$..params$discrete$valid = list(c('word', 'sentence', 'paragraph',
                                               'w', 's', 'p'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'tokenize', event = v$msg, level = "Error")
        stop()
      }

      # Obtain documents and tokenizes
      private$..tokens <- Tokenizer$new()$execute(private$..corpus, tokenUnit)

      event <- paste0("Created ", tokenUnit, " object from Corpus.")
      private$..tokens$message(event)
      private$..corpus$message(event)
      private$logR$log(method = 'tokenize', event = event, level = "Info")
      invisible(self)
    },

    getTokens = function() private$..tokens,

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sample = function(n, name = NULL, stratify = TRUE, replace = FALSE,
                      seed = NULL) {
      # Validation
      private$validate(private$..corpus, methodName = 'sample')

      private$..params <- list()
      private$..params$logicals$variables <- c('stratify', 'replace')
      private$..params$logicals$values <- c(stratify, replace)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'sample', event = v$msg, level = "Error")
        stop()
      }

      # Obtain samples
      sampler <- Sample$new()
      private$..sample <- sampler$execute(x = private$..corpus, n, name, stratify,
                                          replace, seed)

      type <- ifelse(stratify == TRUE, 'stratified', 'non-stratified')
      event <- paste0("Created ", type, " sample from Corpus.")
      private$..corpus$message(event)
      private$..sample$message(event)
      private$logR$log(method = 'sample', event = event, level = "Info")

      invisible(self)
    },

    getSample = function() private$..sample,

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    split = function(name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL) {

      # Validate
      private$validate(private$..corpus, methodName = 'split')

      private$..params <- list()
      private$..params$classes$name <- list('train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c("logical"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'split', event = v$msg, level = "Error")
        stop()
      }

      # Render splits
      splitter <- Split$new()
      private$..splits <- splitter$execute(private$..corpus, name, train, validation, test,
                                stratify, seed)

      event <- paste0("Created cross-validation set from Corpus.")
      private$..splits$message(event)
      private$logR$log(method = 'split', event = event, level = "Info")

      invisible(self)
    },

    getSplits = function(set = NULL) {

      if (is.null(set)) {
        return(private$..splits$getSplits())
      } else if (grepl("^tr", set, ignore.case = TRUE)) {
        return(private$..splits$getTrainingSet())
      } else if (grepl("^v", set, ignore.case = TRUE)) {
        return(private$..splits$getValidationSet())
      } else if (grepl("^t", set, ignore.case = TRUE)) {
        return(private$..splits$getTestSet())
      } else {
        event <- paste0("Set parameter value, ", set, " is invalid. ",
                        "Valid values include c('training', validation', 'test'). ",
                        "See ?", class(self)[1], " for further assistance. ")
        private$logR$log(method = 'split', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFold = function(k = 10, name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL)  {

      # Validate
      private$validate(private$..corpus, methodName = 'kFold')

      private$..params <- list()
      private$..params$classes$name <- list('k', 'train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(k, train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c("logical"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'kFold', event = v$msg, level = "Error")
        stop()
      }
      splitter <- SplitKFold$new()
      private$..kFolds <- splitter$execute(private$..corpus, k, name, train, validation,
                                  test,  stratify, seed)

      event <- paste0("Created k-fold cross-validation set from Corpus.")
      private$..kFolds$message(event)
      private$logR$log(method = 'kFold', event = event, level = "Info")

      return(cvKFold)
    },

    getKFolds = function(k = NULL, set = NULL) {
      return(private$..kFolds$getKFolds(k, set))
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Clean Method                              #
    #-------------------------------------------------------------------------#
    clean = function(config, name = NULL) {

      # Validation
      private$validate(private$..corpus, methodName = 'clean')
      private$..params <- list()
      private$..params$classes$name <- list('config')
      private$..params$classes$objects <- list(config)
      private$..params$classes$valid <- list(c('TextConfig'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'clean', event = v$msg, level = "Error")
        stop()
      }

      ts <- TextStudio$new()
      private$..clean <- ts$loadConfig(config)$execute(private$..corpus, name)

      invisible(self)
    },

    getClean = function() private$..clean

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
