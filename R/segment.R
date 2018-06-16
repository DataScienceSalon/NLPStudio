#' Segment
#'
#' \code{Segment} Segments a Corpus into n equal units of a designated size.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented.}
#'  }
#'
#' @param x Corpus object.
#' @param n Numeric of indicating the number of units to include in each segment.
#' @param size Numeric indicating the size of each units to include in each segment
#' @param stratify Logical. If TRUE (default), Segments will be taken from
#' each document in accordance with the proportions or counts indicated
#' in the 'n' parameter.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Sample Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Segment <- R6::R6Class(
  classname = "Segment",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Sample0,

  private = list(

    ..segments = list(),

    processSegment = function(s, inDocs) {

      outDocs <- private$..segments[[s]]$getDocuments()

      for (j in 1:length(inDocs)) {
        id <- inDocs[[j]]$getId()
        idx <- private$..indices$n[private$..indices$document == id & private$..indices$label == s]
        outDocs[[j]]$content <- inDocs[[j]]$content[idx]
        private$..segments[[s]]$addDocument(outDocs[[j]])
      }
      return(TRUE)
    },

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

      if (length(n) > 1) {
        event <- paste0("Invalid parameter 'n'. Parameter must be of length one.")
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
      private$loadServices("Segment")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Segment Method                              #
    #-------------------------------------------------------------------------#
    execute = function(x, n, name = NULL, stratify = TRUE, seed = NULL) {

      if (!private$validate(x, n, stratify)) stop()

      private$..corpus <- x
      private$..n <- n

      # Build Segment Corpora sans content. Content will be added in getSegments
      for (i in 1:n) {
        private$..segments[[i]] <- Clone$new()$this(x = private$..corpus)
        private$..segments[[i]]$setName(paste0(private$..segments[[i]]$getName(),
                                               " Segment ", i))
      }
      private$segment(n, stratify, seed)

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Get Training Set                              #
    #-------------------------------------------------------------------------#
    getSegments = function(n = NULL) {

      inDocs <- private$..corpus$getDocuments()

      if (is.null(n)) {
        for (i in 1:private$..n) {
          private$processSegment(s = i, inDocs = inDocs)
        }
        return(private$..segments)
      } else {
        private$processSegment(s = n, inDocs = inDocs)
        return(private$..segments[[n]])
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
