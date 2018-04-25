#==============================================================================#
#                                   Clone                                      #
#==============================================================================#
#' Clone
#'
#' \code{Clone0} Abstract class for the Clone family of classes.
#'
#' Abstract class that defines the interface and common methods for the
#' Clone family of classes.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant object.}
#' }
#'
#' @param x Object to be Cloned.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return A Replicant of the input object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Clone Classes
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

    cloneCorpus = function(x, name) {
      out <- Corpus$new()
      out <- private$cloneMeta(x, out)
      if (!is.null(name)) out$setName(name)
      event <- paste0("Corpus, ", out$getName(), ", cloned from ", x$getName(), ".")
      out$message(event = event)
      return(out)
    },

    cloneDocument = function(x, name) {
      out <- Document$new()
      out <- private$cloneMeta(x, out)
      if (!is.null(name)) out$setName(name)
      event <- paste0("Document, ", out$getName(), ", cloned from ", x$getName(), ".")
      out$message(event = event)
      return(out)
    },

    cloneFileSet = function(x, path) {
      out <- FileSet$new()
      out <- private$cloneMeta(x, out)
      if (!is.null(path)) out$setMeta(key = 'path', value = path)
      event <- paste0("FileSet, ", out$getName(), ", cloned from ", x$getName(), ".")
      out$message(event = event)
      return(out)
    },

    cloneFile = function(x, path) {
      out <- File$new()
      out <- private$cloneMeta(x, out)
      if (!is.null(path)) out$setMeta(key = 'path', value = path)
      event <- paste0("File, ", out$getName(), ", cloned from ", x$getName(), ".")
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
    #-------------------------------------------------------------------------#
    #                            Factory Method                               #
    #-------------------------------------------------------------------------#
    this = function(x, path = NULL, name = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus','Document',
                                               "FileSet", "File"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      classname <- class(x)[1]
      out <- switch(classname,
                    Corpus = private$cloneCorpus(x, name = name),
                    Document = private$cloneDocument(x, name = name),
                    FileSet = private$cloneFileSet(x, path = path),
                    File = private$cloneFile(x, path = path))
      return(out)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$koln(self)
    }
  )
)
