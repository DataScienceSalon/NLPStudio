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
  inherit = Sample0,

  private = list(

    #-------------------------------------------------------------------------#
    #                       initCrossValidatinSets                            #
    #-------------------------------------------------------------------------#
    buildCVSets = function(corpusDocs, labels) {

      for (i in 1:length(labels)) {

        # Create the Training, Validation or Test Set Object
        cvSet <-  Clone$new()$this(x = private$..corpus, reference = FALSE)
        cvSet$setName(paste(private$..corpus$getName(),
                                   NLPStudio:::proper(paste(labels[i], "set"))))
        cvSet$setMeta(key = 'cv', value = labels[i], type = 'f')

        # Create and add Document objects to the Set
        for (j in 1:length(corpusDocs)) {

          # Obtain indices for the Document object
          id <- corpusDocs[[j]]$getId()
          idx <- unlist(private$..indices %>%
                          filter(document == id & label == labels[i]) %>%
                          select(n))

          # Create the sample Document object with indexed content from the corpus
          cvDoc <- Clone$new()$this(x = corpusDocs[[j]], content = FALSE)
          cvDoc$content <- corpusDocs[[j]]$content[idx]
          cvDoc$setName(paste(corpusDocs[[j]]$getName(),
                               NLPStudio:::proper(paste(labels[i], "set"))))
          cvDoc$setMeta(key = 'cv', value = labels[i], type = 'f')

          # Add Document object to cvSet object
          cvSet$addDocument(cvDoc)
        }

        # Attach cvSet
        private$attach(cvSet)
      }
      private$..corpus <- NULL
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                              Validate                                   #
    #-------------------------------------------------------------------------#
    validate = function(corpus, train, validation, test, stratify) {
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
        return(FALSE)
      }

      if (sum(train, test, validation) != 1) {
        event <- paste0("Proportions for training, validation and test sets ",
                        "must sum to one. ")
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
      private$loadServices("Split")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Execute                                   #
    #-------------------------------------------------------------------------#
    execute = function(x, name = NULL, train = 0.75, validation = 0,
                          test = 0.25,  stratify = TRUE, seed = NULL) {

      if (!private$validate(x, train, validation, test, stratify)) stop()

      private$..corpus <- x
      if (!is.null(name)) self$setName(name)

      # Format weights and labels,then create of indices of elements to split
      weights <- c(train, validation, test)
      labels <- c('training', 'validation', 'test')
      labels <- labels[weights > 0]
      weights <- weights[weights > 0]
      private$sampleP(labels = labels, seed, stratify, weights = weights)

      # Build and attach CV Set objects from the corpus Document objects.
      corpusDocs <- private$..corpus$getDocuments()
      private$buildCVSets(corpusDocs, labels)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Get Cross-Validation Sets                         #
    #-------------------------------------------------------------------------#
    getSplits = function() { private$..children },

    getTrainingSet = function() {
      train <- private$..children[private$search(key = 'cv', value = 'training')]
      return(train[[1]])
    },

    getValidationSet = function() {
      key <- private$search(key = 'cv', value = 'validation')
      if (sum(key) == 0){
        event <- "There is no validation set for the Corpus."
        private$logR$log(method = 'execute', event = event, level = "Warn")
        return(FALSE)
      } else {
        validation <- private$..children[key]
        return(validation[[1]])
      }
    },

    getTestSet = function() {
      test <- private$..children[private$search(key = 'cv', value = 'test')]
      return(test[[1]])
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$split(self)
    }
  )
)
