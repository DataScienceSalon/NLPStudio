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
    ..training = character(),
    ..validation = character(),
    ..test = character(),

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

      private$..training <- NULL
      private$..validation <- NULL
      private$..test <- NULL
      private$..validationSet <- ifelse(validation == 0, FALSE, TRUE)

      if (!private$validate(x, train, validation, test, stratify)) stop()
      private$..corpus <- x
      private$..name <- name
      private$sampleP(labels = c('training', 'validation', 'test'), seed,
                      stratify, weights = list(c(train, validation, test)))

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Get Training Set                              #
    #-------------------------------------------------------------------------#
    getTrainingSet = function() {

      if (is.null(private$..training)) {

        private$..training <- Clone$new()$this(x = private$..corpus, reference = FALSE)

        # Set Name
        if (is.null(private$..name)) {
          name <- paste(private$..corpus$getName(), "Training Set")
        } else {
          name <- private$..name
        }
        private$..training$setName(name)

        documents <- private$..corpus$getDocuments()

        for (i in 1:length(documents)) {

          document <- Clone$new()$this(x = documents[[i]])
          id <- documents[[i]]$getId()
          idx <- unlist(private$..indices %>%
                          filter(document == id & label == 'training') %>%
                          select(n))
          document$content <- documents[[i]]$content[idx]
          document$setMeta(key = 'cv', value = 'training', type = 'f')
          private$..training$addDocument(document)
        }
      }
      return(private$..training)
    },

    #-------------------------------------------------------------------------#
    #                           Get Validation Set                            #
    #-------------------------------------------------------------------------#
    getValidationSet = function() {

      if (is.null(private$..validation)) {

        if (private$..validationSet) {

          private$..validation <- Clone$new()$this(x = private$..corpus, reference = FALSE)

          # Set Name
          if (is.null(private$..name)) {
            name <- paste(private$..corpus$getName(), "Validation Set")
          } else {
            name <- private$..name
          }
          private$..validation$setName(name)

          documents <- private$..corpus$getDocuments()

          for (i in 1:length(documents)) {

            document <- Clone$new()$this(x = documents[[i]])
            id <- documents[[i]]$getId()
            idx <- unlist(private$..indices %>%
                            filter(document == id & label == 'validation') %>%
                            select(n))
            document$content <- documents[[i]]$content[idx]
            document$setMeta(key = 'cv', value = 'validation', type = 'f')
            private$..validation$addDocument(document)
          }

        } else {
          event <- "No validation set exists."
          private$logR$log(method = 'getValidationSet', event = event, level = "Warn")
        }
      }
      return(private$..validation)
    },
    #-------------------------------------------------------------------------#
    #                             Get Test Set                                #
    #-------------------------------------------------------------------------#
    getTestSet = function() {

      if (is.null(private$..test)) {

        private$..test <- Clone$new()$this(x = private$..corpus, reference = FALSE)

        # Set Name
        if (is.null(private$..name)) {
          name <- paste(private$..corpus$getName(), "Test Set")
        } else {
          name <- private$..name
        }
        private$..test$setName(name)

        documents <- private$..corpus$getDocuments()

        for (i in 1:length(documents)) {

          document <- Clone$new()$this(x = documents[[i]])
          id <- documents[[i]]$getId()
          idx <- unlist(private$..indices %>%
                          filter(document == id & label == 'test') %>%
                          select(n))
          document$content <- documents[[i]]$content[idx]
          document$setMeta(key = 'cv', value = 'test', type = 'f')
          private$..test$addDocument(document)
        }
      }
      return(private$..test)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$split(self)
    }
  )
)
