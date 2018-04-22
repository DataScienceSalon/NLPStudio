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
    cloneDocument = function(x, name = NULL, content) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Document$new()
      out <- private$cloneMeta(x, out)
      out <- out$setMeta(key = 'name', value = name)

      if (content) out$content <- x$content
      event <- paste0("Document cloned from '", x$getName(), "'.")
      out$message(event = event)
      return(out)
    },

    cloneCorpus = function(x, name = NULL, content) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- Corpus$new()
      out <- private$cloneMeta(x, out)
      out <- out$setMeta(key = 'name', value = name)

      # Process Document objects
      docs <- x$getDocuments(key = 'classname', value = 'Document')
      lapply(docs, function(d) {
        doc <- private$cloneDocument(x = d, content)
        out <<- out$addDocument(doc)
      })

      event <- paste0("Corpus cloned from '", x$getName(), "'.")
      out$message(event = event)
      return(out)
    },

    cloneFile = function(x, path, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      x$copy(path)
      out <- File$new(path = path, name = name)
      event <- paste0("File ", x$getName(), " cloned and stored at ", path, ".")
      x$message(event = event)
      out$message(event = event)
      return(out)
    },

    cloneFiles = function(x, path, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      x$copy(path)
      out <- FSSource$new()$dir(x = path, name = name)

      event <- paste0("FileSet ", x$getName()," cloned and stored at ", path, ".")
      x$message(event = event)
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
    this = function(x, path = NULL, name = NULL, content = TRUE) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus','Document',
                                               "FileSet", "File"))
      private$..params$logicals$variables <- c('content')
      private$..params$logicals$values <- c(content)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      classname <- class(x)[1]
      out <- switch(classname,
                    Corpus = private$cloneCorpus(x, name = name, content = content),
                    Document = private$cloneDocument(x, name = name, content = content),
                    FileSet = private$cloneFiles(x, path = path, name = name),
                    File = private$cloneFile(x, path = path, name = name))
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
