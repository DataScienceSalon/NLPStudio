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
    ..owner = character(),
    ..meta = list(
      identity = list(),
      descriptive = list(),
      quant = list(),
      functional = list(),
      admin = list(),
      tech = list()
    ),

    #-------------------------------------------------------------------------#
    #                             Search Method                               #
    #-------------------------------------------------------------------------#
    search = function(key, types = NULL) {

      if (is.null(types)) {
        for (i in 1:length(private$..meta)) {
          if (!is.null(private$..meta[[i]][[key]])) {
            return(private$..meta[[i]][[key]])
          }
        }
        return(NULL)
      } else {
        for (i in 1:length(types)) {
          if (!is.null(private$..meta[[types[i]]][[key]])) {
            return(private$..meta[[types[i]]][[key]])
          }
        }
        return(NULL)
      }
    },

    #-------------------------------------------------------------------------#
    #                           CheckNames Method                             #
    #-------------------------------------------------------------------------#
    checkNames = function(name, type) {
      types2check <- names(private$..meta)[!names(private$..meta) %in% type]
      for (i in 1:length(types2check)) {
        if (name %in% names(private$..meta[[types2check[i]]])) return(FALSE)
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             Setup Metadata                              #
    #-------------------------------------------------------------------------#

    setup = function(x, name = NULL) {

      private$..owner <- x

      # Setup Identity Metadata
      private$..meta$identity$classname <- class(x)[1]
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      private$..meta$identity$id <- toupper(hashids::encode(as.integer(Sys.time()) * 1000000 +
                                                      sample(1:1000, 1, replace = TRUE), settings))

      # Setup Descriptive Metadata
      if (!is.null(name)) {
        private$..meta$descriptive$name <- name
      } else {
        private$..meta$descriptive$name <- private$..meta$identity$id
      }

      # Setup Admin Metadata
      private$..meta$admin$created <- Sys.time()
      private$..meta$admin$createdBy <- Sys.info()[["user"]]
      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- 0
      private$..meta$admin$accessed <- Sys.time()
      private$..meta$admin$accessedBy <- Sys.info()[["user"]]
      private$..meta$admin$nAccessed <- 0
      private$..meta$admin$lastState <- "Instantiated."

      # Setup Technical Metadata
      private$..meta$tech$hardware <- Sys.info()["machine"]
      private$..meta$tech$os <- Sys.info()["sysname"]
      private$..meta$tech$release <- Sys.info()["release"]
      private$..meta$tech$version <- Sys.info()["version"]
      private$..meta$tech$objectSize <- as.character(format(objectSize(x),
                                                            units = "auto"))

      invisible(self)
    }
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this base class.") },

    #-------------------------------------------------------------------------#
    #                             Get Method                                  #
    #-------------------------------------------------------------------------#
    get = function(key = NULL, type = NULL) {

      if (is.null(type) & is.null(key)) return(private$..meta)

      if (!is.null(key)) {
        value <- private$search(key)
        if (is.null(value)) {
          event <- paste0("Variable, ", key, " not found in metadata.")
          private$logR$log(method = 'get', event = event, level = "Warn")
          return(NULL)
        } else {
          return(value)
        }
      } else {

        if (grepl("^i", type, ignore.case = TRUE)) {
          return(private$..meta$identity)
        } else if (grepl("^q", type, ignore.case = TRUE)) {
          return(private$..meta$quant)
        } else if (grepl("^d", type, ignore.case = TRUE)) {
          return(private$..meta$descriptive)
        } else if (grepl("^f", type, ignore.case = TRUE)) {
          return(private$..meta$functional)
        } else if (grepl("^a", type, ignore.case = TRUE)) {
          return(private$..meta$admin)
        } else if (grepl("^t", type, ignore.case = TRUE)) {
          return(private$..meta$tech)
        }
      }
    },

    #-------------------------------------------------------------------------#
    #                       Set Descriptive Method                            #
    #-------------------------------------------------------------------------#
    set = function(key, value, type = 'descriptive') {

      if (grepl("^d", type, ignore.case = TRUE)) {
        type <- 'descriptive'
      } else if (grepl("^f", type, ignore.case = TRUE)) {
        type <- 'functional'
      } else if (grepl("^q", type, ignore.case = TRUE)) {
        type <- 'quant'
      } else {
        event <- paste0("Unable to set metadata of type ", type, ". Permitted ",
                        "types include c('descriptive', 'functional').  See?",
                        class(self)[1], " for further assistance.")
        private$logR$log(method = 'set', event = event, level = "Error")
        stop()
      }

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'set',
                          event = v$msg, level = 'Error')
        stop()
      }

      for (i in 1:length(key)) {
        if (private$checkNames(key[i], type = type))  {
          private$..meta[[type]][[key[i]]] <- value[[i]]
        } else {
          j <- 1
          newVar <- paste0(key[i], "_", j)
          while(private$checkNames(newVar, type = type) == FALSE) {
            j <- j + 1
            newVar <- paste0(key[i], "_", j)
          }
          private$..meta[[type]][[newVar]] <- value[i]
          event <- paste0("Duplicate metadata variable names are not ",
                          "permitted. Variable named ", key[[i]],
                          " was changed to ", newVar, ".")
          private$logR$log(method = "set", event = event,
                           level = "Warn")
        }
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Admin Methods                               #
    #-------------------------------------------------------------------------#
    accessed = function(event = NULL) {

      private$..meta$admin$accessed <- Sys.time()
      private$..meta$admin$accessedBy <- Sys.info()[["user"]]
      private$..meta$admin$nAccessed <- private$..meta$admin$nAccessed + 1
      private$..meta$admin$lastState <- ifelse(is.null(event), "Accessed.", event)

      invisible(self)
    },
    modified = function(event = NULL) {

      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- private$..meta$admin$nModified + 1
      private$..meta$admin$lastState <- ifelse(is.null(event), "Modified.", event)
      private$..meta$tech$objectSize <- as.character(format(objectSize(private$..owner),
                                                            units = "auto"))

      invisible(self)
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
        v <- private$search(key = key[i])
        if (is.null(v)) {
          return(FALSE)
        } else if (v != value[i]) {
          return(FALSE)
        }
      }

      return(TRUE)
    }
  )
)
