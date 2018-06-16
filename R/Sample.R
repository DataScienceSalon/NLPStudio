#' Sample
#'
#' \code{Sample} Creates Corpus from a Corpus sample.
#'
#' Creates a Corpus object from a sample of a Corpus object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented.}
#'  }
#'
#' @param x Corpus object.
#' @param n Numeric of length one, or numeric vector of length equal to
#' the number of documents  in the Corpus.
#' @param stratify Logical. If TRUE (default), samples will be taken from
#' each document in accordance with the proportions or counts indicated
#' in the 'n' parameter.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Sample Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Sample <- R6::R6Class(
  classname = "Sample",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Sample0,

  private = list(
    ..sample = character(),

    validate = function(x, n, stratify) {
      private$..params <- list()
      private$..params$classes$name <- list('x','n', 'stratify')
      private$..params$classes$objects <- list(x, n, stratify)
      private$..params$classes$valid <- list('Corpus', 'numeric', 'logical')
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        return(FALSE)
      }

      if (any (n <= 1) & any(n > 1)) {
        event <- paste0("Invalid variable 'n'. All values must be either less than or ",
                        "equal to one, or all values must be greater than one.")
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
      private$loadServices("Sample")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Sample Method                               #
    #-------------------------------------------------------------------------#
    execute = function(x, n, name = NULL, stratify = TRUE, seed = NULL) {

      if (!private$validate(x, n, stratify)) stop()

      private$..corpus <- x
      private$..sample <- Clone$new()$this(x = private$..corpus)
      if (!is.null(name)) private$..sample$setName(name)

      labels <- c('in', 'out')
      if (any(n <= 1)) {
        weights <- list()
        for (i in 1:length(n)) {
          weights[[i]] <- c(n[i], 1- n[i])
        }
        private$sampleP(labels, seed, stratify, weights)
      } else {
        private$sampleN(n, seed, stratify)
      }
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Get Sample                                  #
    #-------------------------------------------------------------------------#
    getSample = function() {

      inDocs <- private$..corpus$getDocuments()
      outDocs <- private$..sample$getDocuments()

      for (i in 1:length(inDocs)) {
        id <- inDocs[[i]]$getId()
        idx <- private$..indices$n[private$..indices$document == id & private$..indices$label == 'in']
        outDocs[[i]]$content <- inDocs[[i]]$content[idx]
        private$..sample$addDocument(outDocs[[i]])
      }
      return(private$..sample)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$sample(self)
    }
  )
)
