#' CVFactoryHoldOut
#'
#' \code{CVFactoryHoldOut} Creates a hold out CVSet object.
#'
#' Class responsible for creating hold out cross-validation sets or CVSetHoldOut objects.
#' CVSetHoldOut objects contain a training, test and an optional validation test set.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(X, name = NULL)}}{Initializes an object of the CVFactoryHoldOut class.}
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
CVFactoryHoldOut <- R6::R6Class(
  classname = "CVFactoryHoldOut",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CVFactory0,

  private = list(
    splitDocument = function(x, sets, unit, proportions, seed) {

      set.seed(seed)
      splits <- list()

      # Obtain text based upon unit
      if (grepl("^s", unit, ignore.case = TRUE)) {
        text <- Tokens$new(x)$sentence()$get()
      } else  {
        text <- x$text()
      }

      # Create indices and create splits
      idx <- sample(sets, size = length(text), replace = TRUE,
                    prob = proportions)
      for (i in 1:length(proportions)) {
        name <- paste(x$getName(), sets[i], "Set")
        splits[[sets[i]]] <- Clone$new()$this(x = x, name = name)
        splits[[sets[i]]]$content <- text[idx == sets[i]]
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
    build = function(x, unit = "vector", stratify = TRUE, proportions = c(.8,.2),
                     seed = 1) {
      # --------------------Validate parameters-------------------------------#
      private$..params$classes$name <- list('x','seed', 'proportions')
      private$..params$classes$objects <- list(x, seed, proportions)
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

      if (!(length(proportions) %in% c(2,3)) |
          sum(proportions) != 1) {
        event <- paste0("Proportions parameter invalid. The proportions vector ",
                        "must have two or three numeric elements, which sum to one. ")
        private$logR$log(method = 'build', event = event, level = "Error")
        stop()
      }
      # ----------------------------------------------------------------------#
      # Set CV Set types
      if (length(proportions) == 2) {
        sets <- c("Training", "Test")
      } else {
        sets <- c("Training", "Validation", "Test")
      }

      docs <- x$getDocuments()
      # Create single Document object if stratify is FALSE
      if (stratify == FALSE) {
        content <- unlist(lapply(docs, function(d) {
          d$text()
        }))
        docs <- list(Document$new(x = content, name = paste(x$getName(), "Text Document")))
      }

      # Split Documents
      docSplits <- lapply(docs, function(d) {
        private$splitDocument(d, sets, unit, proportions, seed)
      })

      # Create CVSet and CV Objects
      cv <- CV$new(name = paste(x$getName(), "Cross-Validation Sets"))
      for (i in 1:length(sets)) {
        cvSet <- CVSet$new(name = paste(x$getName(), sets[i], "Set"))
        for (j in 1:length(docSplits)) {
          cvSet$addDocument(docSplits[[j]][[sets[i]]])
        }
        cvSet$setMeta(key = 'cv', value = sets[i], type = 'f')
        cv$addCVSet(cvSet)
      }

      return(cv)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvFactoryHoldOut(self)
    }
  )
)
