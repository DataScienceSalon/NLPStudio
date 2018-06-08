#' Split
#'
#' \code{Split} Splits a corpus into a training, test and optional validation set.
#'
#' Splits a corpus into a training, test and optional validation set. These corpora
#' are combined into a single cross-validation set or CVSet object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Split class.}
#'   \item{\code{execute(x, train = 0.75, validation = 0, test = 0.25,
#'   stratify = TRUE, seed = NULL)}}{Executes the corpus splits.}
#'  }
#'
#' @param corpus Corpus object.
#' @param name Character string indicating the name for the cross-validation set.
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
#' @return CVSet object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Split Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Split <- R6::R6Class(
  classname = "Split",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    ..corpus = character(),
    ..unseed = integer(),

    splitCorpus = function(name, train, validation, test,  stratify, seed) {

      # Get vector of cross validation set names and weights
      cvSets <- c("Training", "Validation", "Test")
      cvSets <- cvSets[c(train, validation, test) > 0]
      weights <- c(train, validation, test)
      weights <- weights[weights > 0]
      nSets <- sum(c(train, validation, test) > 0)

      # Segment private$..corpus
      corpora <- NLPStudio::segment(private$..corpus, nSets, weights, stratify, seed)
      if (length(seed) > 0) {
        private$..unseed <- private$..unseed + 1
        set.seed(private$..unseed)
      }

      # Create CVSet
      cvSetName <- name
      if (is.null(name)) cvSetName <- paste(private$..corpus$getName(), "CVSet")
      cvSet <- CVSet$new(name = cvSetName)

      # Update metadata
      for (i in 1:length(cvSets)) {
        private$..corpusName <- paste(corpora[[i]]$getName(), cvSets[[i]], "Set")
        corpora[[i]]$setName(name = private$..corpusName)
        corpora[[i]]$setMeta(key = 'cv', value = paste(cvSets[[i]], "Set"), type = 'f')
        documents <- corpora[[i]]$getDocuments()
        for (j in 1:length(documents)) {
          docName <- paste(documents[[j]]$getName(), cvSets[[i]], "Set")
          documents[[j]]$setName(name = docName)
          documents[[j]]$setMeta(key = 'cv', value = paste(cvSets[[i]], "Set"), type = 'f')
          corpora[[i]]$addDocument(documents[[j]])
        }
        cvSet <- cvSet$addCorpus(corpora[[i]])
      }

      # Update CVSet MetaData
      cvSet$setMeta(key = cvSets, value = weights, type = 'q')
      return(cvSet)
    },

    validate = function(corpus, train, validation, test, stratify, seed) {
      private$..params <- list()
      private$..params$classes$name <- list('corpus','train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(corpus, train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('Corpus'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c("logical"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        stop()
      }

      if (sum(train, test, validation) != 1) {
        event <- paste0("Proportions for training, validation and test sets ",
                        "must equal one. ")
        private$logR$log(method = 'execute', event = event, level = "Error")
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
      private$loadServices()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Split Method                                #
    #-------------------------------------------------------------------------#
    execute = function(corpus, name = NULL, train = 0.75, validation = 0,
                       test = 0.25,  stratify = TRUE, seed = NULL) {

      private$validate(corpus, train, validation, test, stratify, seed)
      private$..corpus <- corpus
      private$..unseed <- seed
      cvSet <- private$splitCorpus(name, train, validation, test,  stratify, seed)
      private$..corpus <- NULL
      return(cvSet)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$split(self)
    }
  )
)
