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
    splitDocument = function(x, corpora, train, validation, test, seed) {

      # Set seed (if requested) and sampling probabilities
      if (!is.null(seed)) { set.seed(seed) }
      prob <- c(train,validation, test)
      if (validation == 0) prob <- c(train, test)

      # Create indices and splits
      idx <- sample(corpora, size = length(x$content), replace = TRUE,
                    prob = prob)

      splits <- list()
      for (i in 1:length(prob)) {
        name <- paste(x$getName(), corpora[i], "Set")
        splits[[corpora[i]]] <- Clone$new()$this(x = x, reference = FALSE)
        splits[[corpora[i]]]$setName(name)
        splits[[corpora[i]]]$content <- x$content[idx == corpora[i]]
      }

      return(splits)
    },

    validate = function(corpus, train, validation, test, stratify, seed) {
      private$..params <- list()
      private$..params$classes$name <- list('corpus','train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(corpus, train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('Corpus'),
                                             c('numeric', 'numeric', 'numeric'),
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
    },

    getDocuments = function(corpus, stratify) {
      docs <- corpus$getDocuments()
      # Create single Document object if stratify is FALSE
      if (stratify == FALSE) {
        content <- unlist(lapply(docs, function(d) {
          d$content
        }))
        docs <- list(Document$new(x = content, name = paste(corpus$getName(), "Document")))
      }
      return(docs)
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

      corpora <- c("Training", "Validation", "Test")
      if (validation == 0) corpora <- c("Training", "Rest")

      documents <- private$getDocuments(corpus, stratify)

      # Split Documents
      docSplits <- lapply(documents, function(d) {
        private$splitDocument(d, corpora, train, validation, test, seed)
      })

      # Combine Document objects into Corpus objects, then into a single CVSet
      if (is.null(name)) name <- corpus$getName()
      cvSet <- CVSet$new(name = name)
      for (i in 1:length(corpora)) {
        corpus <- Clone$new()$this(corpus, reference = FALSE)
        corpus$setName(name = paste(name, corpora[i], "Set"))
        for (j in 1:length(docSplits)) {
          corpus$addDocument(docSplits[[j]][[corpora[i]]])
        }
        corpus$setMeta(key = 'cv', value = corpora[i], type = 'f')
        cvSet$addCorpus(corpus)
      }

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
