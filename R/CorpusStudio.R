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

    #-------------------------------------------------------------------------#
    #                    Corpus Validation Method                             #
    #-------------------------------------------------------------------------#
    validate = function(x, methodName) {

      private$..params <- list()
      private$..params$classes$name <- list('corpus')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName,
                         event = v$msg, level = "Error")
        stop()
      }
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
    #                       Corpus Sourcing Methods                           #
    #-------------------------------------------------------------------------#
    sourceDir = function(path, name = NULL) {
      return(CSourceDir$new()$source(x = path, name))
    },

    sourceQ = function(corpus, name = NULL) {
      return(CSourceQ$new()$source(corpus, name))
    },

    sourceTM = function(corpus, name = NULL) {
      return(CSourceTM$new()$source(corpus, name))
    },

    sourceVector = function(x, name = NULL) {
      return(CSourceV$new()$source(x, name))
    },

    #-------------------------------------------------------------------------#
    #                       Corpus Tokenize Methods                           #
    #-------------------------------------------------------------------------#
    tokenize = function(corpus, tokenizer = 'tokenizer', tokenUnit = 'sentence') {

      private$validate(corpus, methodName = 'tokenize')

      if (grepl("^c", tokenUnit, ignore.case = TRUE)) {
        tokens <- Token$new()$chars(corpus, tokenizer)
      } else if (grepl("^w", tokenUnit, ignore.case = TRUE)) {
        tokens <- Token$new()$words(corpus, tokenizer)
      } else if (grepl("^s", tokenUnit, ignore.case = TRUE)) {
        tokens <- Token$new()$sentences(corpus, tokenizer)
      } else if (grepl("^p", tokenUnit, ignore.case = TRUE)) {
        tokens <- Token$new()$paragraphs(corpus, tokenizer)
      } else {
        event <- "Invalid tokenUnit."
        private$logR$log(method = 'tokenize', event = event, level = "Error")
        stop()
      }
      return(tokens)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sample = function(corpus, sampleSize, stratify = TRUE, replace = FALSE,
                      seed = NULL, name = NULL) {

      private$validate(corpus, methodName = 'sample')

      sampler <- Sample$new()
      sample <- sampler$execute(x = corpus,
                                n = sampleSize,
                                name = name,
                                stratify = stratify,
                                replace = replace,
                                seed = seed)$getSample()

      return(sample)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Clean Method                              #
    #-------------------------------------------------------------------------#
    clean = function(corpus, config, name = NULL) {

      private$validate(corpus, methodName = 'clean')

      ts <- TextStudio$new(config)
      cleanCorpus <- ts$execute(corpus, name)

      return(cleanCorpus)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    split = function(corpus, train = .7, validation = .15, test = .15,
                     stratify = TRUE, seed = NULL, name = NULL) {

      private$validate(corpus, methodName = 'split')

      splits <- Split$new()$execute(corpus,
                                    name = name,
                                    train = train,
                                    validation = validation,
                                    test = test,
                                    stratify = stratify,
                                    seed = seed)$getCVSet()

      return(splits)
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFold = function(corpus, name = NULL, k = 10, stratify = TRUE, seed = NULL)  {

      private$validate(corpus, methodName = 'kFold')

      folds <- KFold$new()$execute(corpus,
                                   k = k,
                                   name = name,
                                   stratify = stratify,
                                   seed = seed)$getKFolds()

      return(folds)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
