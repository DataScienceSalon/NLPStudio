#' CVHoldOut
#'
#' \code{CVHoldOut} Creates a hold out CVSet object.
#'
#' Class responsible for creating hold out cross-validation sets or CVSetHoldOut objects.
#' CVSetHoldOut objects contain a training, test and an optional validation test set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CVHoldOut class.}
#'   \item{\code{execute()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param x Corpus object for which the CVSetHoldOut object is to be created.
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
CVHoldOut <- R6::R6Class(
  classname = "CVHoldOut",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    splitDocument = function(x, corpora, proportions, seed) {

      if (!is.null(seed)) { set.seed(seed) }
      splits <- list()

      # Create indices and create splits
      idx <- sample(corpora, size = length(x$content), replace = TRUE,
                    prob = proportions)
      for (i in 1:length(proportions)) {
        name <- paste(x$getName(), corpora[i], "Set")
        splits[[corpora[i]]] <- Clone$new()$this(x = x, reference = FALSE)
        splits[[corpora[i]]]$setName(name)
        splits[[corpora[i]]]$content <- x$content[idx == corpora[i]]
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
    #                             Split Method                                #
    #-------------------------------------------------------------------------#
    split = function(x, stratify = TRUE, proportions = c(.8,.2),
                     seed = NULL, name = NULL) {
      # --------------------Validate parameters-------------------------------#
      private$..params$classes$name <- list('x','seed', 'proportions')
      private$..params$classes$objects <- list(x, seed, proportions)
      private$..params$classes$valid <- list(c('Corpus'),
                                             c('integer', 'numeric', 'NULL'),
                                             c("numeric"))
      private$..params$discrete$valid <- list(c("vector", "sentence", "v", "s"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'split',
                         event = v$msg, level = "Error")
        stop()
      }

      if (!(length(proportions) %in% c(2,3)) |
          sum(proportions) != 1) {
        event <- paste0("Proportions parameter invalid. The proportions vector ",
                        "must have two or three numeric elements, which sum to one. ")
        private$logR$log(method = 'build', event = event, level = "Error")
        stop()
      }
      # ----------------------------------------------------------------------#
      # Set Corpora types
      if (length(proportions) == 2) {
        corpora <- c("Training", "Test")
      } else {
        corpora <- c("Training", "Validation", "Test")
      }

      docs <- x$getDocuments()
      # Create single Document object if stratify is FALSE
      if (stratify == FALSE) {
        content <- unlist(lapply(docs, function(d) {
          d$content
        }))
        docs <- list(Document$new(x = content, name = paste(x$getName(), "Text Document")))
      }

      # Split Documents
      docSplits <- lapply(docs, function(d) {
        private$splitDocument(d, corpora, proportions, seed)
      })

      # Create CVSet, and Corpora objects
      if (!is.null(name)) name <- x$getName()
      cvSet <- CVSet$new(name = paste(name, "Hold-Out Cross-Validation Set"))
      for (i in 1:length(corpora)) {
        corpus <- Clone$new()$this(x, reference = FALSE)
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
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvHoldOut(self)
    }
  )
)
