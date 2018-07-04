#------------------------------------------------------------------------------#
#                               CorpusStudio                                   #
#------------------------------------------------------------------------------#
#' CorpusStudio
#'
#' \code{CorpusStudio}  Creates a Corpus object then prepares it for cross-validation downstream.
#'
#' Class responsible for creating, cleaning, sampling, splitting and
#' constructing the cross-validation object that will be used by downstream
#' modeling classes. This is performed in five states.
#'
#' The first stage builds the corpus object from one of several sources:
#' a directory source, a FileSet object, a TM Corpus object, or a
#' quanteda corpus object. The second stage is optional and reshapes the
#' Corpus object into word, sentence or paragraph units. The third stage,
#' the Sampling Stage takes a stratified or non-stratfified sampling
#' from the Corpus object. The forth stage, the cross-validation stage,
#' produces one of two cross-validation objects: a CVSet object, which
#' is comprised of a training, test and optional validation set, or
#' a CVSetKFold object which contains k-folds, each comprised of a
#' training and test set. The cross-validation product is the final product
#' and forms the data basis for the modeling phase.
#'
#' @param x a series of character vectors, each containing the text for a single
#' document, a FileSet object containing .txt files, a character string
#' containing the directory holding .txt files, a quanteda corpus object, or
#' a tm VCorpus, or tm SimpleCorpus object.#'
#' @param name Character string containing the name to assign to the final
#' CVSet or CVSetKFold object.
#' @param cv The type of cross-validation product to deliver. Valid values
#' are c('standard', 'kFold'). The default is standard and one letter
#' abbreviations are acceptable.
#' @param textConfig a TextConfig object which encapsulates the text
#' cleaning configuration.
#' @param n Numeric parameter used by the sample method. It contains the number
#' of samples to obtain from the Corpus or the proportion of the Corpus to sample
#' prior to splitting into cross-validation set(s).
#' @param k Numeric. If 'cv' is 'kFold', this number indicates the number
#' of folds to produce.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param replace Logical. If TRUE, sampling is conducted with replacement. The
#' default is FALSE.
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
    ..settings = list(
      x = character(),
      name = character(),
      tokenize = logical(),
      tokenUnits = character(),
      clean = logical(),
      textConfig = character(),
      sample = logical(),
      stratify = logical(),
      sampleSize = numeric(),
      replace = logical(),
      cv = character(),
      train = numeric(),
      validation = numeric(),
      test = numeric(),
      k = numeric(),
      seed = numeric()
    ),
    ..corpus = character(),
    ..tokens = character(),
    ..sample = character(),
    ..clean  = character(),
    ..cvSet = character(),
    ..kFolds = character(),

    #-------------------------------------------------------------------------#
    #                         Build Corpus Method                             #
    #-------------------------------------------------------------------------#
    build = function() {

      sourceType <- class(private$..settings$x)[1]
      if (sourceType == 'character') {
        if (isDirectory(private$..settings$x)) {
          sourceType <- 'directory'
        }
      }

      x <- private$..settings$x
      name <- private$..settings$name

      private$..corpus <- switch(sourceType,
                                 FileSet = CSourceFileSet$new()$source(x, name),
                                 corpus = CSourceQ$new()$source(x, name),
                                 VCorpus = CSourceTM$new()$source(x, name),
                                 SimpleCorpus = CSourceTM$new()$source(x, name),
                                 character = CSourceV$new()$source(x, name),
                                 directory = CSourceDir$new()$source(x, name))

      event <- paste0("Constructed Corpus from ", sourceType, ".")
      private$..corpus$message(event)
      private$logR$log(method = 'build', event = event, level = "Info")
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Tokenize Method                             #
    #-------------------------------------------------------------------------#
    tokenizeCorpus = function() {
      private$..tokens <- Tokenizer$new()$execute(private$..corpus,
                                                  private$..settings$tokenUnit)

      event <- paste0("Created ", private$..settings$tokenUnit, " object from Corpus.")
      private$..tokens$message(event)
      private$..corpus$message(event)
      private$logR$log(method = 'tokenize', event = event, level = "Info")
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sampleCorpus = function() {

      if (private$..settings$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      # Obtain samples
      sampler <- Sample$new()
      private$..sample <- sampler$execute(x = corpus,
                                          n = private$..settings$sampleSize,
                                          name = private$..settings$name,
                                          stratify = private$..settings$stratify,
                                          replace = private$..settings$replace,
                                          seed = private$..settings$seed)$getSample()

      type <- ifelse(private$..settings$stratify == TRUE,
                     'stratified', 'non-stratified')
      event <- paste0("Created ", type, " Corpus sample.")
      private$..corpus$message(event)
      private$..sample$message(event)
      private$logR$log(method = 'sample', event = event, level = "Info")

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Clean Method                              #
    #-------------------------------------------------------------------------#
    cleanCorpus = function() {

      if (private$..settings$sample) {
        corpus <- private$..sample
        event <- paste0("Cleaned corpus sample object")
      } else if (private$..settings$tokenize) {
        corpus <- private$..tokens
        event <- paste0("Cleaned tokenized Corpus object")
      } else {
        corpus <- private$..corpus
        event <- paste0("Cleaned corpus object")
      }

      ts <- TextStudio$new()$loadConfig(private$..settings$textConfig)
      private$..clean <- ts$execute(corpus, private$..settings$name)$getCorpus()

      private$logR$log(method = 'cleanCorpus', event = event, level = "Info")

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    splitCorpus = function() {

      if (private$..settings$clean) {
        corpus <- private$..clean
      } else if (private$..settings$sample) {
        corpus <- private$..sample
      } else if (private$..settings$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      # Render splits
      private$..cvSet <- Split$new()$execute(corpus,
                                             name = private$..settings$name,
                                             train = private$..settings$train,
                                             validation = private$..settings$validation,
                                             test = private$..settings$test,
                                             stratify = private$..settings$stratify,
                                             seed = private$..settings$seed)$getCVSet()

      event <- paste0("Created cross-validation set from Corpus.")
      private$logR$log(method = 'split', event = event, level = "Info")

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFoldCorpus = function()  {

      if (private$..settings$clean) {
        corpus <- private$..clean
      } else if (private$..settings$sample) {
        corpus <- private$..sample
      } else if (private$..settings$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      private$..kFolds <- SplitKFold$new()$execute(corpus,
                                                   k = private$..settings$k,
                                                   name = private$..settings$name,
                                                   stratify = private$..settings$stratify,
                                                   seed = private$..settings$seed)$getKFolds()
      event <- paste0("Created k-fold cross-validation set from Corpus.")
      private$logR$log(method = 'kFold', event = event, level = "Info")

      return(TRUE)
    }
  ),

  active = list(
    #-------------------------------------------------------------------------#
    #                          Settings Manager                               #
    #-------------------------------------------------------------------------#
    name = function(x) {
      if (missing(x)) {
        return(private$..settings$name)
      } else {
        private$..settings$name <- x
        invisible(self)
      }
    },

    tokenize = function(x) {
      if (missing(x)) {
        return(private$..settings$tokenize)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('tokenize')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'tokenize', event = v$msg, level = "Error")
        } else {
          private$..settings$tokenize <- x
        }
        invisible(self)
      }
    },

    tokenUnits = function(x) {
      if (missing(x)) {
        return(private$..settings$tokenUnits)
      } else {
        private$..params <- list()
        private$..params$discrete$variables = list('tokenUnits')
        private$..params$discrete$values = list(x)
        private$..params$discrete$valid = list(c('word', 'sentence', 'paragraph',
                                                 'w', 's', 'p'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'tokenUnits', event = v$msg, level = "Error")
        } else {
          private$..settings$tokenUnits <- x
        }
        invisible(self)
      }
    },

    clean = function(x) {
      if (missing(x)) {
        return(private$..settings$clean)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('clean')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'clean', event = v$msg, level = "Error")
        } else {
          private$..settings$clean <- x
        }
        invisible(self)
      }
    },

    textConfig = function(x) {
      if (missing(x)) {
        return(private$..settings$textConfig)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('textConfig')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('TextConfig'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'textConfig', event = v$msg, level = "Error")
        } else {
          private$..settings$textConfig <- x
        }
        invisible(self)
      }
    },

    sample = function(x) {
      if (missing(x)) {
        return(private$..settings$sample)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('sample')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'sample', event = v$msg, level = "Error")
        } else {
          private$..settings$sample <- x
        }
        invisible(self)
      }
    },

    stratify = function(x) {
      if (missing(x)) {
        return(private$..settings$stratify)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('stratify')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'stratify', event = v$msg, level = "Error")
        } else {
          private$..settings$stratify <- x
        }
        invisible(self)
      }
    },

    sampleSize = function(x) {
      if (missing(x)) {
        return(private$..settings$sampleSize)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('sampleSize')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'sampleSize', event = v$msg, level = "Error")
        } else {
          private$..settings$sampleSize <- x
        }
        invisible(self)
      }
    },

    replace = function(x) {
      if (missing(x)) {
        return(private$..settings$replace)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('replace')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'replace', event = v$msg, level = "Error")
        } else {
          private$..settings$replace <- x
        }
        invisible(self)
      }
    },

    cv = function(x) {
      if (missing(x)) {
        return(private$..settings$cv)
      } else {
        private$..params <- list()
        private$..params$discrete$variables = list('cv')
        private$..params$discrete$values = list(x)
        private$..params$discrete$valid = list(c('standard', 'kFold'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'cv', event = v$msg, level = "Error")
        } else {
          private$..settings$cv <- x
        }
        invisible(self)
      }
    },

    train = function(x) {
      if (missing(x)) {
        return(private$..settings$train)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('train')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        private$..params$range$variable <- c('x')
        private$..params$range$value <- c(x)
        private$..params$range$low <- 0
        private$..params$range$high <- 1
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'train', event = v$msg, level = "Error")
        } else {
          private$..settings$train <- x
        }
        invisible(self)
      }
    },

    validation = function(x) {
      if (missing(x)) {
        return(private$..settings$validation)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('validation')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        private$..params$range$variable <- c('x')
        private$..params$range$value <- c(x)
        private$..params$range$low <- 0
        private$..params$range$high <- 1
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'validation', event = v$msg, level = "Error")
        } else {
          private$..settings$validation <- x
        }
        invisible(self)
      }
    },

    test = function(x) {
      if (missing(x)) {
        return(private$..settings$test)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('test')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        private$..params$range$variable <- c('x')
        private$..params$range$value <- c(x)
        private$..params$range$low <- 0
        private$..params$range$high <- 1
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'test', event = v$msg, level = "Error")
        } else {
          private$..settings$test <- x
        }
        invisible(self)
      }
    },

    k = function(x) {
      if (missing(x)) {
        return(private$..settings$k)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('k')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        private$..params$range$variable <- c('k')
        private$..params$range$value <- c(x)
        private$..params$range$low <- 2
        private$..params$range$high <- 10
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'k', event = v$msg, level = "Error")
        } else {
          private$..settings$k <- x
        }
        invisible(self)
      }
    },

    seed = function(x) {
      if (missing(x)) {
        return(private$..settings$seed)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('seed')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'seed', event = v$msg, level = "Error")
        } else {
          private$..settings$seed <- x
        }
        invisible(self)
      }
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL, tokenize = TRUE,
                          tokenUnits = 'sentence', clean = TRUE,
                          textConfig = NULL, sample = FALSE, stratify = TRUE,
                          sampleSize = 1, replace = FALSE, cv = 'standard',
                          train = 0.75, validation = 0.0, test = 0.25,
                          k = 5, seed = NULL) {

      private$loadServices(name = 'CorpusStudio')

      private$..params <- list()
      private$..params$classes$name <- list('x', 'sampleSize')
      private$..params$classes$objects <- list(x, sampleSize)
      private$..params$classes$valid = list(c('character', 'FileSet', 'corpus',
                                              'VCorpus', 'SimpleCorpus'),
                                            c('numeric'))
      private$..params$discrete$variables = list('tokenUnits', 'cv')
      private$..params$discrete$values = list(tokenUnits, cv)
      private$..params$discrete$valid = list(c('word', 'sentence', 'paragraph',
                                              'w', 's', 'p'),
                                             c('standard', 'kFold'))
      private$..params$logicals$variables <- c( 'tokenize', 'clean', 'sample',
                                                'stratify', 'replace')
      private$..params$logicals$values <- c(tokenize, clean, sample, stratify,
                                            replace)

      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      if (clean & class(textConfig)[1] != "TextConfig") {
        event <- paste0("TextConfig object is missing or invalid. If 'clean' is ",
                        "TRUE, a TextConfig object must be provided. See ?",
                        class(self)[1], " for further assistance.")
        private$logR$log(method = 'initialize', event = event, level = "Error")
        stop()
      }

      if (cv == 'standard') {
        if (sum(train, validation, test) != 1) {
          event <- paste0("The proportions for training, validation, and test sets ",
          "must sum to one.")
          private$logR$log(method = 'initialize', event = event, level = "Error")
          stop()
        }
      }

      private$..settings$x = x
      private$..settings$name = name
      private$..settings$tokenize = tokenize
      private$..settings$tokenUnits = tokenUnits
      private$..settings$clean = clean
      private$..settings$textConfig = textConfig
      private$..settings$sample = sample
      private$..settings$stratify = stratify
      private$..settings$sampleSize = sampleSize
      private$..settings$replace = replace
      private$..settings$cv = cv
      private$..settings$train = train
      private$..settings$validation = validation
      private$..settings$test = test
      private$..settings$k = k
      private$..settings$seed = seed

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Execute Method                              #
    #-------------------------------------------------------------------------#
    execute = function() {
      private$build()
      if (private$..settings$tokenize) private$tokenizeCorpus()
      if (private$..settings$sample) private$sampleCorpus()
      if (private$..settings$clean) private$cleanCorpus()
      if (private$..settings$cv == 'standard') private$splitCorpus()
      if (private$..settings$cv == 'kFold') private$kFoldCorpus()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Object Accessors                              #
    #-------------------------------------------------------------------------#
    getCorpus = function() private$..corpus,
    getTokens = function() private$..tokens,
    getSample = function() private$..sample,
    getClean = function() private$..clean,
    getCVSet = function() private$..cvSet,
    getKFolds = function() private$..kFolds,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
