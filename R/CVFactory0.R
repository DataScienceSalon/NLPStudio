#' CVFactory0
#'
#' \code{CVFactory0} Abstract factory class for the family of  cross validation factory classes.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this abstract class.}
#'   \item{\code{build()}}{Executes the process of sourcing the Corpus object.}
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
#' @param k The number of K-Fold validation sets to create.
#' @param seed Integer used to set the seed for random sampling
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Cross Validation Classes
#' @export
CVFactory0 <- R6::R6Class(
  classname = "CVFactory0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    splitDocument = function(x, proportions, seed) {

      if (length(proportions) == 2) {
        sets <- c("Training", "Test")
      } else if (length(proportions) == 3) {
        sets <- c("Training", "Validation", "Test")
      } else {
        sets <- paste0("Fold-", seq(1:length(proportions)))
      }

      set.seed(seed)
      splits <- list()
      text <- x$text()
      idx <- sample(sets, size = length(text), replace = TRUE,
                    prob = proportions)
      for (i in 1:length(proportions)) {
        name <- paste(x$getName(), sets[i], "Set")
        splits[[i]] <- Clone$new()$this(x = x, name = name)
        splits[[i]]$content <- text[idx == sets[i]]
      }

      return(splits)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {stop("Method not implemented for this abstract class")},
    build = function(x, unit = "vector", stratify = TRUE, proportions = c(.8,.2),
                     k = 0, seed = 1) {
      # Validate parameters
      private$..params$classes$name <- list('x','k', 'seed', 'proportions')
      private$..params$classes$objects <- list(x, k, seed, proportions)
      private$..params$classes$valid <- list(c('Corpus'), c('integer', 'numeric'),
                                             c('integer', 'numeric'), c("numeric"))
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

      if (k > 0) proportions <- rep(1/k, k)

    }
  )
)
