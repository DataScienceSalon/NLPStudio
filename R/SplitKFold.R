#' SplitKFold
#'
#' \code{SplitKFold} Creates a K-Fold cross-validation set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented.}
#'  }
#'
#' @param x Corpus object.
#' @param k Numeric of folds
#' @param stratify Logical. If TRUE (default), SplitKFolds will be taken from
#' each document in accordance with the proportions or counts indicated
#' in the 'n' parameter.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Sample Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
SplitKFold <- R6::R6Class(
  classname = "SplitKFold",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Sample0,

  private = list(
    ..folds = list(),

    initKFolds = function() {

      name <- private$..corpus$getName()

      for (i in 1:private$..k) {
        private$..folds[[i]] <- list()
        private$..folds[[i]]$training <- Clone$new()$this(x = private$..corpus)
        private$..folds[[i]]$training$setName(paste0(name, " Fold ", i, " Training Set"))
        private$..folds[[i]]$test <- Clone$new()$this(x = private$..corpus)
        private$..folds[[i]]$test$setName(paste0(name, " Fold ", i, " Test Set"))
      }
      return(TRUE)
    },

    processFold = function(k, inDocs) {

      trainDocs <- private$..folds[[k]]$training$getDocuments()
      testDocs <- private$..folds[[k]]$test$getDocuments()

      for (j in 1:length(inDocs)) {
        id <- inDocs[[j]]$getId()
        trainIdx <- private$..indices$n[private$..indices$document == id & private$..indices$label != k]
        testIdx <- private$..indices$n[private$..indices$document == id & private$..indices$label == k]

        trainDocs[[j]]$content <- inDocs[[j]]$content[trainIdx]
        testDocs[[j]]$content <- inDocs[[j]]$content[testIdx]

        private$..folds[[k]]$training$addDocument(trainDocs[[j]])
        private$..folds[[k]]$test$addDocument(testDocs[[j]])
      }
      return(TRUE)
    },

    validate = function(x, k, stratify) {
      private$..params <- list()
      private$..params$classes$name <- list('x','k', 'stratify')
      private$..params$classes$objects <- list(x, k, stratify)
      private$..params$classes$valid <- list('Corpus', 'numeric', 'logical')
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        return(FALSE)
      }

      if (length(k) > 1) {
        event <- paste0("Invalid parameter 'k'. Parameter must be of length one.")
        private$logR$log(method = 'execute', event = event, level = "Error")
        return(FALSE)
      }
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices("SplitKFold")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             SplitKFold Method                               #
    #-------------------------------------------------------------------------#
    execute = function(x, k, name = NULL, stratify = TRUE, seed = NULL) {

      if (!private$validate(x, k, stratify)) stop()

      private$..corpus <- x
      private$..k <- k

      private$initKFolds()
      private$segment(k, stratify, seed)

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                                Get KFolds                               #
    #-------------------------------------------------------------------------#
    getKFolds = function(k = NULL, set = NULL) {

      inDocs <- private$..corpus$getDocuments()

      if (is.null(k)) {
        for (i in 1:private$..k) {
          private$processFold(k = i, inDocs = inDocs)
        }
        return(private$..folds)
      } else {
        private$processFold(k = k, inDocs = inDocs)

        if (is.null(set)) {
          return(private$..folds[[k]])
        } else if (grepl("^tr", tolower(set), ignore.case = TRUE)) {
          return(private$..folds[[k]]$training)
        } else {
          return(private$..folds[[k]]$test)
        }
      }
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$segment(self)
    }
  )
)
