#' Meta0
#'
#' \code{Meta0} Base class for metadata objects.
#'
#' Provides the methods for creating, managing, and reporting metadata. This
#' base functionality is shared by all the Metadata family classes.
#'
#' @section Meta0 methods:
#' @template metadataMethods
#'
#' @template metadataParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
Meta0 <- R6::R6Class(
  classname = "Meta0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..meta = list(
      identity = list(),
      descriptive = list(),
      quant = list(),
      functional = list(),
      admin = list(),
      tech = list()
    ),

    #-------------------------------------------------------------------------#
    #                           Identity Metadata                             #
    #-------------------------------------------------------------------------#

    setIdentity = function(x, name = NULL) {

      # Designate family, class and purpose
      private$..meta$identity$family <- class(x)[2]
      private$..meta$identity$class <- class(x)[1]

      # Creates unique identifier
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      private$..meta$identity$id <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                                                      sample(1:1000, 1, replace = TRUE), settings)

      # Designate/create object name
      private$..meta$identity$name <- ifelse(is.null(name),
                                             paste0(private$..meta$identity$class,
                                                    " (", toupper(private$..meta$identity$id),
                                                    ")"), name)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Administrative Metadata                           #
    #-------------------------------------------------------------------------#
    setAdmin = function() {

      private$..meta$admin$created <- Sys.time()
      private$..meta$admin$createdBy <- Sys.info()[["user"]]
      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- 0
      private$..meta$admin$accessed <- Sys.time()
      private$..meta$admin$accessedBy <- Sys.info()[["user"]]
      private$..meta$admin$nAccessed <- 0
      private$..meta$admin$lastState <- "Instantiated."

      return(TRUE)
    }
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this base class.") },
    getMeta = function() { return(private$..meta) },

    #-------------------------------------------------------------------------#
    #                            Identity Methods                             #
    #-------------------------------------------------------------------------#
    getIdentity = function(key = NULL) {

      if (!is.null(key)) {
        if (key == 'id') { return(private$..meta$identity$id) }
        if (key == 'name') { return(private$..meta$identity$name) }
      }
      return(private$..meta$identity)

    },

    #-------------------------------------------------------------------------#
    #                         Descriptive Metadata Methods                    #
    #-------------------------------------------------------------------------#
    getDescriptive = function(key = NULL) {

      if (length(private$..meta$descriptive) == 0) {
        return(NULL)
      } else if (is.null(key)) {
        return(private$..meta$descriptive)
      } else if (!is.null(private$..meta$descriptive[[key]])) {
        return(private$..meta$descriptive[[key]])
      } else {
        event <- paste0("Key, '", key, "', is not a valid descriptive metadata ",
                        "variable. See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'getDescriptive',
                         event = event, level = "Warn")
        return(FALSE)
      }
    },

    setDescriptive = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'setDescriptive',
                          event = v$msg, level = 'Error')
        stop()
      }

      for (i in 1:length(key)) {
        private$..meta$descriptive[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Quant Methods                               #
    #-------------------------------------------------------------------------#
    getQuant = function() { return(private$..meta$quant) },

    setQuant = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'setQuant',
                          event = v$msg, level = "Error")
        stop()
      }

      for (i in 1:length(key)) {
        private$..meta$quant[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Functional  Methods                           #
    #-------------------------------------------------------------------------#
    getFunctional = function() { return(private$..meta$functional) },

    setFunctional = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'setFunctional',
                          event = v$msg, level = "Error")
        stop()
      }

      for (i in 1:length(key)) {
        private$..meta$functional[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Admin Methods                               #
    #-------------------------------------------------------------------------#
    getAdmin = function() { return(private$..meta$admin) },

    modified = function(event = NULL) {

      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- private$..meta$admin$nModified + 1
      private$..meta$admin$accessed <- Sys.time()
      private$..meta$admin$accessedBy <- Sys.info()[["user"]]
      private$..meta$admin$nAccessed <- private$..meta$admin$nAccessed + 1
      private$..meta$admin$lastState <- ifelse(is.null(event), "Modified.", event)

      invisible(self)
    },

    accessed = function(event = NULL) {

      private$..meta$admin$accessed <- Sys.time()
      private$..meta$admin$accessedBy <- Sys.info()[["user"]]
      private$..meta$admin$nAccessed <- private$..meta$admin$nAccessed + 1

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Technical Metadata                             #
    #-------------------------------------------------------------------------#
    getTech = function() { private$..meta$tech },

    setTech = function(x, fileName = NULL, directory = NULL, url = NULL) {

      private$..meta$tech$hardware <- Sys.info()["machine"]
      private$..meta$tech$os <- Sys.info()["sysname"]
      private$..meta$tech$release <- Sys.info()["release"]
      private$..meta$tech$version <- Sys.info()["version"]
      private$..meta$tech$size <- as.character(format(object.size(x),
                                                      units = "auto"))
      private$..meta$tech$fileName <- fileName
      private$..meta$tech$directory <- directory
      if (!is.null(fileName) & !is.null(fileSource)) {
        private$..meta$tech$fileSize <- file.size(file.path(fileSource, fileName))
      }
      private$..meta$tech$url <- url

      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                             Query Methods                               #
    #-------------------------------------------------------------------------#
    query = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- FALSE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'query',
                         event = v$msg, level = "Warn")
      }

      if (length(key) != length(value)) {
        key <- rep(key, length(value))
      }

      for (i in 1:length(key)) {
        for (j in 1:length(private$..meta)) {
          if (!is.null(private$..meta[[j]][[key[i]]])) {
            if (private$..meta[[j]][[key[i]]] %in% value[i])
              return(TRUE)
          }
        }
      }

      return(FALSE)
    }
  )
)
