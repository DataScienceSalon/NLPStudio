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

      out$setName(name = x$name)

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

    cloneCVSet = function(x, reference) {
      out <- CVSet$new()
      out <- private$cloneMeta(x, out)
      corpora <- x$getCorpus()
      for (i in 1:length(corpora)) {
        corpus <- private$cloneCorpus(corpora[[i]], reference)
        out$addCorpus(corpora[[i]])
      }
      event <- paste0("CVSet, ", x$getName(), ", cloned.")
      out$message(event = event)
      return(out)
    },

    cloneCorpus = function(x, reference) {
      out <- Corpus$new()
      out <- private$cloneMeta(x, out)
      if (reference) {
        docs <- x$getDocuments()
        for (i in 1:length(docs)) {
          doc <- private$cloneDocument(docs[[i]])
          out$addDocument(doc)
        }
      }
      event <- paste0("Corpus, ", x$getName(), ", cloned.")
      out$message(event = event)
      return(out)
    },

    cloneDocument = function(x) {
      out <- Document$new(x$content)
      out <- private$cloneMeta(x, out)
      event <- paste0("Document, ", x$getName(), ", cloned.")
      x$message(event = event)
      return(out)
    },

    cloneFileSet = function(x, reference, path) {
      name <- x$getName()
      out <- FileSet$new(name = name)
      out <- private$cloneMeta(x, out)
      if (reference) {
        files <- x$getFiles()
        if (length(files) > 0) {
          for (i in 1:length(files)) {
            fileName <- files[[i]]$getFileName()
            filePath <- file.path(path,fileName)
            file <- private$cloneFile(files[[i]], filePath)
            out$addFile(file)
          }
        }
      }
      event <- paste0("FileSet, ", x$getName(), ", cloned.")
      x$message(event = event)
      return(out)
    },

    cloneFile = function(x, path) {
      fs <- FileStudio$new()
      fs$copy(x, path)
      out <- File$new(path)
      out <- private$cloneMeta(x, out)
      out$setFilePath(path)
      event <- paste0("File, ", x$getName(), ", cloned.")
      x$message(event = event)
      return(out)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                            Factory Method                               #
    #-------------------------------------------------------------------------#
    this = function(x, reference = TRUE, path = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$logical$variables = list('reference')
      private$..params$logical$values = list(reference)
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus','Document',
                                               "FileSet", "File", "CVSet"))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      if (class(x)[[1]] %in% c("FileSet", "File") & is.null(path)) {
        event <- paste0("The path variable must be provided for FileSet and ",
                        "File objects.")
        private$logR$log(method = 'this',
                         event = event, level = "Error")
        stop()
      }

      classname <- class(x)[1]
      out <- switch(classname,
                    CVSet = private$cloneCVSet(x, reference),
                    Corpus = private$cloneCorpus(x, reference),
                    Document = private$cloneDocument(x),
                    FileSet = private$cloneFileSet(x, reference, path),
                    File = private$cloneFile(x, path))
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
