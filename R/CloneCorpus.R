#==============================================================================#
#                               CloneCorpus                                    #
#==============================================================================#
#' CloneCorpus
#'
#' \code{CloneCorpus} Class responsible for cloning Corpus and Document objects.
#'
#' Class responsible for cloning Corpus and Document objects.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant object.}
#' }
#'
#' @param x Object to be cloned. Currently supports Corpus and Document objects.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return A Replicant of the input object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @export
CloneCorpus <- R6::R6Class(
  "CloneCorpus",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Clone0,

  private = list(

    cloneDocument = function(x, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Document$new()
      out <- private$cloneMeta(x, out)
      out <- out$setMeta(key = 'name', value = name)

      out$content <- x$content
      event <- paste0("CloneCorpusd from '", x$getName(), "'.")
      out$message(event = event)
      return(out)
    },

    cloneCorpus = function(x, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Corpus$new()
      out <- private$cloneMeta(x, out)
      out <- out$setMeta(key = 'name', value = name)

      # Process Document objects
      docs <- x$getDocument(key = 'classname', value = 'Document')
      lapply(docs, function(d) {
        doc <- private$cloneDocument(x = d)
        out <<- out$addDocument(doc)
      })

      event <- paste0("CloneCorpusd from '", x$getName(), "'.")
      out$message(event = event)
      return(out)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    document = function(x, name = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Document')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'document',
                         event = v$msg, level = "Error")
        stop()
      }

      out <- private$cloneDocument(x = x, name = name)
      return(out)
    },

    corpus = function(x, name = NULL) {

      # Validate parameters
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'corpus',
                         event = v$msg, level = "Error")
        stop()
      }

      out <- private$cloneCorpus(x = x, name = name)
      return(out)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cloneCorpus(self)
    }
  )
)
