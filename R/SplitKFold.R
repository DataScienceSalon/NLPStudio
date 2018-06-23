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

    processFold = function(k, corpusDocs, name) {

      # Instantiate Fold and Training and Test Corpus objects
      fold <- Fold$new(name)
      fold <- Copy$new()$this(x = private$..corpus, to = fold)
      train <- Clone$new()$this(x = private$..corpus, reference = FALSE)
      test <- Clone$new()$this(x = private$..corpus, reference = FALSE)

      # Set names
      if (is.null(name))  name <- private$..corpus$getName()
      fold$setName(paste0(name, " Fold #", k))
      train$setName(paste0(name, " Fold #", k, " Training Set"))
      test$setName(paste0(name, " Fold #", k, " Test Set"))

      # Set cross-validation type
      fold$setMeta(key = 'cv', value = paste0("Fold #", k), type = 'f')
      train$setMeta(key = 'cv', value = 'training', type = 'f')
      test$setMeta(key = 'cv', value = 'test', type = 'f')

      # Create and add training and test Documents to Corpora object
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

      fold$addSet(train)
      fold$addSet(test)

      private$attach(fold)

      event <- paste0("Fold #", k, " created.")
      fold$message(event)
      private$logR$log(method = 'processFold', event = event, level = "Info")

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
    #                             SplitKFold Method                           #
    #-------------------------------------------------------------------------#
    execute = function(x, k, name = NULL, stratify = TRUE, seed = NULL) {

      if (!private$validate(x, k, stratify)) stop()

      private$..corpus <- x
      private$..k <- k

      private$segment(k, stratify, seed)
      corpusDocs <- private$..corpus$getDocuments()
      for (i in 1:private$..k) { private$processFold(k = i, corpusDocs = corpusDocs,
                                                     name = name) }

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                                Get KFolds                               #
    #-------------------------------------------------------------------------#
    getFolds = function(k = NULL) {

      if (is.null(k)) {
        return(private$..children)
      } else {
        return(private$..children[[k]])
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
