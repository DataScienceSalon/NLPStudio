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
#' @param k Numeric of cvSets
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

    processFold = function(k, name) {

      # Instantiate cvSet
      cvSet <- CVSet$new(name)
      cvSet <- Copy$new()$this(x = private$..corpus, to = cvSet)
      train <- Clone$new()$this(x = private$..corpus, reference = FALSE)
      test <- Clone$new()$this(x = private$..corpus, reference = FALSE)

      # Set names
      if (is.null(name)) name <- private$..corpus$getName()
      cvSet$setName(paste0(name, " Fold #", k))
      train$setName(paste0(name, " Fold #", k, " Training Set"))
      test$setName(paste0(name, " Fold #", k, " Test Set"))

      # Set cross-validation type
      cvSet$setMeta(key = 'cv', value = paste0("Fold #", k), type = 'f')
      train$setMeta(key = 'cv', value = 'training', type = 'f')
      test$setMeta(key = 'cv', value = 'test', type = 'f')

      # Create and add training and test Documents to Corpora object
      corpusDocs <- private$..corpus$getDocuments()
      for (i in 1:length(corpusDocs)) {
        id <- corpusDocs[[i]]$getId()
        trainIdx <- private$..indices$n[private$..indices$document == id & private$..indices$label != k]
        testIdx <- private$..indices$n[private$..indices$document == id & private$..indices$label == k]

        trainDoc <- Clone$new()$this(x = corpusDocs[[i]], content = FALSE)
        testDoc <- Clone$new()$this(x = corpusDocs[[i]], content = FALSE)

        trainDoc$setName(paste0(corpusDocs[[i]]$getName(), "Training Document"))
        testDoc$setName(paste0(corpusDocs[[i]]$getName(), "Test Document"))

        trainDoc$setMeta(key = 'cv', value = 'training', type = 'f')
        testDoc$setMeta(key = 'cv', value = 'test', type = 'f')

        trainDoc$content <- corpusDocs[[i]]$content[trainIdx]
        testDoc$content <- corpusDocs[[i]]$content[testIdx]

        train$addDocument(trainDoc)
        test$addDocument(testDoc)
      }

      cvSet$addCorpus(train)
      cvSet$addCorpus(test)

      private$..cvSetKFold$addCVSet(cvSet)

      event <- paste0("Fold #", k, " created.")
      cvSet$message(event)
      private$logR$log(method = 'processFold', event = event, level = "Info")

      return(TRUE)
    },

    buildFolds = function(k, name) {

      cvSetName <- name
      if (is.null(cvSetName)) cvSetName <- paste0(private$..corpus$getName(),
                                             " KFold CV Set")

      private$..cvSetKFold <- CVSetKFold$new(name = cvSetName)

      for (i in 1:k) {
        private$processFold(k = i, name = name)
      }

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
    #                             SplitKFold Method                           #
    #-------------------------------------------------------------------------#
    execute = function(x, k, name = NULL, stratify = TRUE, seed = NULL) {

      if (!private$validate(x, k, stratify)) stop()

      private$..corpus <- x

      private$segment(k, stratify, seed)
      private$buildFolds(k, name)

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                                Get KFolds                               #
    #-------------------------------------------------------------------------#
    getKFolds = function() private$..cvSetKFold,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$splitKFold(self)
    }
  )
)
