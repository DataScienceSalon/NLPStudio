#' CVFactoryKFold
#'
#' \code{CVFactoryKFold} Creates a hold out CVSet object.
#'
#' Class responsible for creating hold out cross-validation sets or CVSetKFold objects.
#' CVSetKFold objects contain a training, test and an optional validation test set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CVFactoryKFold class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param x Corpus object for which the CVSetKFold object is to be created.
#' @param unit Character string indicating whether the unit for random sampling is
#' the vector or sentence. If the latter, the documents will be tokenized into sentences
#' prior to sampling.
#' @param stratify Logical. If TRUE, sampling will be conducted at the individual
#' document level. Otherwise, the Document objects will be combined into a single
#' Document object prior to sampling.
#' @param proportions Numeric vector of length 2 or 3, indicating the proportions to allocate
#' to the training (validation) and test sets.  If the length of the vector is 2, a
#' training and test set will be created in accordance with the proportions indicated. If
#' the length is 3, a validation set will be created in addition to the training and
#' test sets.
#' @param seed
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVFactoryKFold <- R6::R6Class(
  classname = "CVFactoryKFold",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CVFactory0,

  private = list(
    splitDocument = function(x, k, sets, unit, proportions, seed) {

      # Obtain text based upon unit
      if (grepl("^s", unit, ignore.case = TRUE)) {
        text <- Tokenizer$new()$this(x = x, type = 'sentence')$get()
      } else  {
        text <- x$text
      }

      # Create text folds
      set.seed(seed)
      idx <- sample.int(length(sets), size = length(text), replace = TRUE,
                    prob = proportions)
      numFolds <- seq(1:k)
      folds <- lapply(numFolds, function(i) {
        text[idx == i]
      })

      # Create cross-validation splits
      splits <- list()
      for (i in 1:length(folds)) {
        name <- paste(x$getName(), sets[i])
        splits[[sets[i]]]$training <- Clone$new()$this(x = x, reference = FALSE)
        splits[[sets[i]]]$validation <- Clone$new()$this(x = x, reference = FALSE)
        splits[[sets[i]]]$training$setName(name = paste(name, "Training Set"))
        splits[[sets[i]]]$validation$setName(name = paste(name, "Validation Set"))
        splits[[sets[i]]]$training$text <- unlist(folds[-i])
        splits[[sets[i]]]$validation$text <- unlist(folds[i])
      }

      return(splits)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(x, unit = "vector", stratify = FALSE, k = 10, seed = 1) {
      # --------------------Validate parameters-------------------------------#
      private$..params$classes$name <- list('x','seed', 'k')
      private$..params$classes$objects <- list(x, seed, k)
      private$..params$classes$valid <- list(c('Corpus'),  c('integer', 'numeric'),
                                             c("numeric"))
      private$..params$discrete$variables <- c('unit')
      private$..params$discrete$values <- c(unit)
      private$..params$discrete$valid <- list(c("vector", "sentence", "v", "s"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build',
                         event = v$msg, level = "Error")
        stop()
      }

      # ----------------------------------------------------------------------#
      # Set CV Folds
      sets <- paste0("CVSet-KFold-", seq(1:k))
      proportions <- rep(1/k, k)

      docs <- x$getDocuments()
      # Create single Document object if stratify is FALSE
      if (stratify == FALSE) {
        content <- unlist(lapply(docs, function(d) {
          d$text
        }))
        docs <- list(Document$new(x = content, name = paste(x$getName(), "Text Document")))
      }

      # Split Documents
      docSplits <- lapply(docs, function(d) {
        splits <- private$splitDocument(d, k, sets, unit, proportions, seed)
      })

      # Create CV, CVSet and corpora
      cv <- CV$new(name = paste(x$getName(), "Cross-Validation Sets"))
      for (i in 1:length(sets)) {

        # Create CVSet and training and validation corpora
        cvSet <- CVSet$new(name = paste0(x$getName(), " K-Fold CV Set-", i))
        train <- Clone$new()$this(x = x, reference = FALSE)
        train$setName(name = paste0(x$getName(), " K-Fold CV Set-", i,  " Training Set"))
        train$setMeta(key = 'cv', value = 'Training')
        validation <- Clone$new()$this(x = x, reference = FALSE)
        validation$setName(name = paste0(x$getName(), " K-Fold CV Set-", i,  " Validation Set"))
        validation$setMeta(key = 'cv', value = 'Validation')
        for (j in 1:length(docSplits)) {
          train$addDocument(docSplits[[j]][[i]]$training)
          validation$addDocument(docSplits[[j]][[i]]$validation)
        }
        cvSet$addCorpus(x = train)
        cvSet$addCorpus(x = validation)
        cv$addCVSet(cvSet)
      }
      return(cv)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvFactoryKFold(self)
    }
  )
)
