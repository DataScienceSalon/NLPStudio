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
    ..parameters = list(
      x = character(),
      name = character(),
      tokenize = logical(),
      tokenizer = character(),
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
    ..splits = character(),
    ..kFolds = character(),

    #-------------------------------------------------------------------------#
    #                         Build Corpus Method                             #
    #-------------------------------------------------------------------------#
    build = function() {

      sourceType <- class(private$..parameters$x)[1]
      if (sourceType == 'character') {
        filepath <- "(^\\.+[/]((\\w.+)))+.(\\w)+"
        if (grepl(filepath, private$..parameters$x, perl = TRUE)) {
          sourceType <- 'directory'
        }
      }

      x <- private$..parameters$x
      name <- private$..parameters$name

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

      tokenizer <- private$..parameters$tokenizer
      tokenUnit <- private$..parameters$tokenUnit

      if (grepl("^c", tokenUnit, ignore.case = TRUE)) {
        private$..tokens <- Token$new(private$..corpus)$chars(tokenizer)$getTokens()
      } else if (grepl("^w", tokenUnit, ignore.case = TRUE)) {
        private$..tokens <- Token$new(private$..corpus)$words(tokenizer)$getTokens()
      } else if (grepl("^s", tokenUnit, ignore.case = TRUE)) {
        private$..tokens <- Token$new(private$..corpus)$sentences(tokenizer)$getTokens()
      } else if (grepl("^p", tokenUnit, ignore.case = TRUE)) {
        private$..tokens <- Token$new(private$..corpus)$paragraphs(tokenizer)$getTokens()
      } else {
        event <- "Invalid tokenUnit."
        private$logR$log(method = 'tokenize', event = event, level = "Error")
        stop()
      }

      event <- paste0("Created ", tokenUnit, " object from Corpus.")
      private$..tokens$message(event)
      private$..corpus$message(event)
      private$logR$log(method = 'tokenizeCorpus', event = event, level = "Info")
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sampleCorpus = function() {

      if (private$..parameters$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      # Obtain samples
      sampler <- Sample$new()
      private$..sample <- sampler$execute(x = corpus,
                                          n = private$..parameters$sampleSize,
                                          name = private$..parameters$name,
                                          stratify = private$..parameters$stratify,
                                          replace = private$..parameters$replace,
                                          seed = private$..parameters$seed)$getSample()

      type <- ifelse(private$..parameters$stratify == TRUE,
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

      if (private$..parameters$sample) {
        corpus <- private$..sample
        event <- paste0("Cleaned corpus sample object")
      } else if (private$..parameters$tokenize) {
        corpus <- private$..tokens
        event <- paste0("Cleaned tokenized Corpus object")
      } else {
        corpus <- private$..corpus
        event <- paste0("Cleaned corpus object")
      }

      ts <- TextStudio$new()$loadConfig(private$..parameters$textConfig)
      private$..clean <- ts$execute(corpus, private$..parameters$name)$getClean()

      private$logR$log(method = 'cleanCorpus', event = event, level = "Info")

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    splitCorpus = function() {

      if (private$..parameters$clean) {
        corpus <- private$..clean
      } else if (private$..parameters$sample) {
        corpus <- private$..sample
      } else if (private$..parameters$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      # Render splits
      private$..splits <- Split$new()$execute(corpus,
                                             name = private$..parameters$name,
                                             train = private$..parameters$train,
                                             validation = private$..parameters$validation,
                                             test = private$..parameters$test,
                                             stratify = private$..parameters$stratify,
                                             seed = private$..parameters$seed)$getCVSet()

      event <- paste0("Created cross-validation set from Corpus.")
      private$logR$log(method = 'split', event = event, level = "Info")

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFoldCorpus = function()  {

      if (private$..parameters$clean) {
        corpus <- private$..clean
      } else if (private$..parameters$sample) {
        corpus <- private$..sample
      } else if (private$..parameters$tokenize) {
        corpus <- private$..tokens
      } else {
        corpus <- private$..corpus
      }

      private$..kFolds <- KFold$new()$execute(corpus,
                                                   k = private$..parameters$k,
                                                   name = private$..parameters$name,
                                                   stratify = private$..parameters$stratify,
                                                   seed = private$..parameters$seed)$getKFolds()
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
        return(private$..parameters$name)
      } else {
        private$..parameters$name <- x
        invisible(self)
      }
    },

    tokenize = function(x) {
      if (missing(x)) {
        return(private$..parameters$tokenize)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('tokenize')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'tokenize', event = v$msg, level = "Error")
        } else {
          private$..parameters$tokenize <- x
        }
        invisible(self)
      }
    },

    tokenizer = function(x) {
      if (missing(x)) {
        return(private$..parameters$tokenizer)
      } else {
        private$..params <- list()
        private$..params$discrete$variables = list('tokenizer')
        private$..params$discrete$values = list(x)
        private$..params$discrete$valid = list(c('openNLP', 'quanteda', 'tokenizer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'tokenizer', event = v$msg, level = "Error")
        } else {
          private$..parameters$tokenizer <- x
        }
        invisible(self)
      }
    },


    tokenUnits = function(x) {
      if (missing(x)) {
        return(private$..parameters$tokenUnits)
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
          private$..parameters$tokenUnits <- x
        }
        invisible(self)
      }
    },

    clean = function(x) {
      if (missing(x)) {
        return(private$..parameters$clean)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('clean')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'clean', event = v$msg, level = "Error")
        } else {
          private$..parameters$clean <- x
        }
        invisible(self)
      }
    },

    textConfig = function(x) {
      if (missing(x)) {
        return(private$..parameters$textConfig)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('textConfig')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('TextConfig'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'textConfig', event = v$msg, level = "Error")
        } else {
          private$..parameters$textConfig <- x
        }
        invisible(self)
      }
    },

    sample = function(x) {
      if (missing(x)) {
        return(private$..parameters$sample)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('sample')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'sample', event = v$msg, level = "Error")
        } else {
          private$..parameters$sample <- x
        }
        invisible(self)
      }
    },

    stratify = function(x) {
      if (missing(x)) {
        return(private$..parameters$stratify)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('stratify')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'stratify', event = v$msg, level = "Error")
        } else {
          private$..parameters$stratify <- x
        }
        invisible(self)
      }
    },

    sampleSize = function(x) {
      if (missing(x)) {
        return(private$..parameters$sampleSize)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('sampleSize')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'sampleSize', event = v$msg, level = "Error")
        } else {
          private$..parameters$sampleSize <- x
        }
        invisible(self)
      }
    },

    replace = function(x) {
      if (missing(x)) {
        return(private$..parameters$replace)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('replace')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'replace', event = v$msg, level = "Error")
        } else {
          private$..parameters$replace <- x
        }
        invisible(self)
      }
    },

    split = function(x) {
      if (missing(x)) {
        return(private$..parameters$split)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('split')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'split', event = v$msg, level = "Error")
        } else {
          private$..parameters$split <- x
        }
        invisible(self)
      }
    },

    cv = function(x) {
      if (missing(x)) {
        return(private$..parameters$cv)
      } else {
        private$..params <- list()
        private$..params$logicals$variables <- c('cv')
        private$..params$logicals$values <- c(x)
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'cv', event = v$msg, level = "Error")
        } else {
          private$..parameters$cv <- x
        }
        invisible(self)
      }
    },

    train = function(x) {
      if (missing(x)) {
        return(private$..parameters$train)
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
          private$..parameters$train <- x
        }
        invisible(self)
      }
    },

    validation = function(x) {
      if (missing(x)) {
        return(private$..parameters$validation)
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
          private$..parameters$validation <- x
        }
        invisible(self)
      }
    },

    test = function(x) {
      if (missing(x)) {
        return(private$..parameters$test)
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
          private$..parameters$test <- x
        }
        invisible(self)
      }
    },

    k = function(x) {
      if (missing(x)) {
        return(private$..parameters$k)
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
          private$..parameters$k <- x
        }
        invisible(self)
      }
    },

    seed = function(x) {
      if (missing(x)) {
        return(private$..parameters$seed)
      } else {
        private$..params <- list()
        private$..params$classes$name <- list('seed')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid = list(c('numeric', 'integer'))
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'seed', event = v$msg, level = "Error")
        } else {
          private$..parameters$seed <- x
        }
        invisible(self)
      }
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL, tokenize = TRUE, tokenizer = 'openNLP',
                          tokenUnits = 'sentence', clean = TRUE,
                          textConfig = NULL, sample = FALSE, stratify = TRUE,
                          sampleSize = 1, replace = FALSE, split = TRUE,
                          train = 0.75, validation = 0.0, test = 0.25,
                          cv = FALSE,  k = 5, seed = NULL) {

      private$loadServices(name = 'CorpusStudio')

      private$..params <- list()
      private$..params$classes$name <- list('x', 'sampleSize', 'k')
      private$..params$classes$objects <- list(x, sampleSize, k)
      private$..params$classes$valid = list(c('character', 'FileSet', 'corpus',
                                              'VCorpus', 'SimpleCorpus'),
                                            c('numeric'), c('numeric'))
      private$..params$discrete$variables = list('tokenUnits','tokenizer')
      private$..params$discrete$values = list(tokenUnits, tokenizer)
      private$..params$discrete$valid = list(c('word', 'sentence', 'paragraph',
                                              'w', 's', 'p'),
                                             c('openNLP', 'quanteda', 'tokenizer'))
      private$..params$logicals$variables <- c( 'tokenize', 'clean', 'sample',
                                                'stratify', 'replace',
                                                'split', 'cv')
      private$..params$logicals$values <- c(tokenize, clean, sample, stratify,
                                            replace, split, cv)

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


      if (split) {
        if (sum(train, validation, test) != 1) {
          event <- paste0("The proportions for training, validation, and test sets ",
          "must sum to one.")
          private$logR$log(method = 'initialize', event = event, level = "Error")
          stop()
        }
      }

      private$..parameters$x <- x
      private$..parameters$name <- name
      private$..parameters$tokenize <- tokenize
      private$..parameters$tokenizer <- tokenizer
      private$..parameters$tokenUnits <- tokenUnits
      private$..parameters$clean <- clean
      private$..parameters$textConfig <- textConfig
      private$..parameters$sample <- sample
      private$..parameters$stratify <- stratify
      private$..parameters$sampleSize <- sampleSize
      private$..parameters$replace <- replace
      private$..parameters$split <- split
      private$..parameters$train <- train
      private$..parameters$validation <- validation
      private$..parameters$test <- test
      private$..parameters$cv <- cv
      private$..parameters$k <- k
      private$..parameters$seed <- seed

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Execute Method                              #
    #-------------------------------------------------------------------------#
    execute = function() {
      private$build()
      if (private$..parameters$tokenize) private$tokenizeCorpus()
      if (private$..parameters$sample) private$sampleCorpus()
      if (private$..parameters$clean) private$cleanCorpus()
      if (private$..parameters$split) private$splitCorpus()
      if (private$..parameters$cv) private$kFoldCorpus()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Object Accessors                              #
    #-------------------------------------------------------------------------#
    getCorpus = function() private$..corpus,
    getTokens = function() private$..tokens,
    getSample = function() private$..sample,
    getClean = function() private$..clean,
    getSplits = function() private$..splits,
    getKFolds = function() private$..kFolds,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
