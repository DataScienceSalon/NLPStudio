#==============================================================================#
#                                   Clone                                      #
#==============================================================================#
#' Clone
#'
#' \code{Clone} Class responsible for cloning primitive and composite objects.
#'
#' Class clones primitive and composite objects. Currently supports
#' Document and Corpus objects.
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
Clone <- R6::R6Class(
  "Clone",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Super,

  private = list(

    cloneMeta = function(x, out) {

      # Clone descriptive
      descriptive <- x$getMeta(type = 'd')
      if (length(descriptive) > 0){
        keys <- names(descriptive)
        for (i in 1:length(descriptive)) {
          out$setMeta(key = keys[i], value = descriptive[[i]], type = 'd')
        }
      }

      # Clone functional
      functional <- x$getMeta(type = 'f')
      if (length(functional) > 0) {
        keys <- names(functional)
        for (i in 1:length(functional)) {
          out$setMeta(key = keys[i], value = functional[[i]], type = 'f')
        }
      }
      return(out)
    },

    cloneDocument = function(x, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Document$new()
      out <- private$cloneMeta(x, out)
      out <- out$setMeta(key = 'name', value = name)

      out$content <- x$content
      event <- paste0("Cloned from '", x$getName(), "'.")
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

      event <- paste0("Cloned from '", x$getName(), "'.")
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
