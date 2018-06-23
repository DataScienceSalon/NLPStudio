#' Sample0
#'
#' \code{Sample0} Base class for Sample and Split Classes
#'
#' Base class that defines the methods common to the Split and Sample Classes.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented.}
#'  }
#'
#' @param corpus Corpus object.
#' @param name Character string indicating the name for the cross-validation set.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Sample Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Sample0 <- R6::R6Class(
  classname = "Sample0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Composite0,

  private = list(

    ..corpus = character(),
    ..indices = data.frame(),

    segment = function(n, stratify, seed) {

      documents <- private$..corpus$getDocuments()

      if (length(seed) > 0)  {
        set.seed(seed)
        unseed <- seed + 1
      }

      if (stratify) {
        if (length(n) == 1) n <- rep(n, length(documents))
        private$..indices <- rbindlist(lapply(seq_along(documents), function(d) {
          segments = list()
          segments$document <- rep(documents[[d]]$getId(), length(documents[[d]]$content))
          segments$n <- seq(1:length(documents[[d]]$content))
          segments$label <- sample(n[[d]], size = length(documents[[d]]$content), replace = TRUE)
          segments
        }))
      } else {
        private$..indices <- rbindlist(lapply(documents, function(d) {
          segments <- list()
          segments$document <- rep(d$getId(), length(d$content))
          segments$n <- seq(1:length(d$content))
          segments
        }))
        private$..indices$label <- sample(n, size = nrow(private$..indices), replace = TRUE)
      }
      if (length(seed) > 0)  set.seed(unseed)

    },

    sampleN = function(n, seed, stratify, replace) {

      documents <- private$..corpus$getDocuments()

      if (length(seed) > 0)  {
        set.seed(seed)
        unseed <- seed + 1
      }

      if (stratify) {
        if (length(n) == 1) n <- rep(ceiling(n/length(documents)), length(documents))
        private$..indices <- rbindlist(lapply(seq_along(documents), function(d) {
          split <- list()
          split$document <- rep(documents[[d]]$getId(), length(documents[[d]]$content))
          split$n <- seq(1:length(documents[[d]]$content))
          idx <- sample.int(n = length(documents[[d]]$content), size = n[[d]], replace = replace)
          split$label[split$n %in% idx] <- 'in'
          split$label[!split$n %in% idx] <- 'out'
          split
        }))
      } else {
        private$..indices <- rbindlist(lapply(documents, function(d) {
          split <- list()
          split$document <- rep(d$getId(), length(d$content))
          split$n <- seq(1:length(d$content))
          split$label <- rep('out', length(d$content))
          split
        }))
        idx <- sample.int(n = nrow(private$..indices), size = n, replace = replace)
        private$..indices$label[idx] <- 'in'
      }
      if (length(seed) > 0)  set.seed(unseed)

      return(TRUE)
    },

    sampleP = function(labels, seed, stratify, weights) {

      documents <- private$..corpus$getDocuments()

      if (length(seed) > 0)  {
        set.seed(seed)
        unseed <- seed + 1
      }

      if (stratify) {

        private$..indices <- rbindlist(lapply(seq_along(documents), function(d) {
          split <- list()
          split$document <- rep(documents[[d]]$getId(), length(documents[[d]]$content))
          split$n <- seq(1:length(documents[[d]]$content))
          split$label <- sample(labels, size = length(documents[[d]]$content), replace = TRUE,
                                  prob = weights[[1]])
          split
        }))
      } else {
        private$..indices <- rbindlist(lapply(documents, function(d) {
          split <- list()
          split$document <- rep(d$getId(), length(d$content))
          split$n <- seq(1:length(d$content))
          split
        }))
        private$..indices$label <- sample(labels, size = nrow(private$..indices), replace = TRUE,
                                         prob = weights[[1]])
      }

      if (length(seed) > 0)  set.seed(unseed)

      return(TRUE)
    }
  ),

  public = list(
    initialize = function() { stop("This method is not implemented for this abstract class") }
  )
)
