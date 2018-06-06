#' SplitKFold
#'
#' \code{SplitKFold} Splits a Corpus object into a k fold cross-validation sets.
#'
#' Splits a Corpus object into k fold cross-validation sets, each containing
#' a training, test and an optional validation set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the SplitKFold class.}
#'   \item{\code{execute(x, train = 0.75, validation = 0, test = 0.25,
#'   stratify = TRUE, seed = NULL)}}{Executes the corpus splits.}
#'  }
#'
#' @param corpus Corpus object.
#' @param k Numeric indicating the number of folds.
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
#' @return CVKFold object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Split Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
SplitKFold <- R6::R6Class(
  classname = "SplitKFold",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split,

  private = list(

    ..folds = character(),
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    splitFolds = function(name, train, validation, test, stratify, seed) {

      # Create CVKFold Object
      if (is.null(name))  name <- paste(private$..folds[[1]]$getName(),
                                        "K Fold Cross-Validation Set")
      cvKFold <- CVKFold$new(name)

      # Process Folds
      for (i in 1:length(private$..folds)) {
        private$..corpus <- private$..folds[[i]]
        cvSet <- private$splitCorpus(name, train, validation, test,  stratify, seed)
        cvKFold$addCVSet(cvSet)
      }
      private$..corpus <- NULL
      private$..folds <- NULL
      return(cvKFold)
    },

    validate = function(corpus, k, train, validation, test, stratify, seed) {
      private$..params <- list()
      private$..params$classes$name <- list('corpus','k', 'train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(corpus, k, train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('Corpus'),
                                             c('integer', 'numeric'),
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
    #                             SplitKFold Method                                #
    #-------------------------------------------------------------------------#
    execute = function(corpus, k, name = NULL, train = 0.75, validation = 0,
                       test = 0.25,  stratify = TRUE, seed = NULL) {

      private$validate(corpus, k, train, validation, test, stratify, seed)
      private$..folds <- NLPStudio::segment(corpus, n = k)
      cvKFold <- private$splitFolds(name, train, validation, test, stratify, seed)

      return(cvKFold)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$splitKFold(self)
    }
  )
)
