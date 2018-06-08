#' Meta
#'
#' \code{Meta} Base class for metadata objects.
#'
#' Provides the methods for creating, managing, and reporting metadata. This
#' base functionality is shared by all the Metadata family classes.
#'
#' @section Meta methods:
#' @template metadataMethods
#'
#' @template metadataParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
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
      settings <- hashids::hashid_settings(salt = "some salt, or garlic sliced thinly",
                                           min_length = 12)
      private$..meta$identity$id <- hashids::encode(as.integer(Sys.time()) * 1000 +
                                                      sample(1:10000, 1, replace = TRUE), settings)

      # Setup Descriptive Metadata
      if (!is.null(name)) {
        private$..meta$identity$name <- name
      } else {
        private$..meta$identity$name <- private$..meta$identity$id
      }

      # Setup Admin Metadata
      private$..meta$admin$created <- Sys.time()
      private$..meta$admin$createdBy <- Sys.info()[["user"]]
      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- 0
      private$..meta$admin$lastState <- "Instantiated."

      # Setup Technical Metadata
      private$..meta$tech$hardware <- Sys.info()["machine"]
      private$..meta$tech$os <- Sys.info()["sysname"]
      private$..meta$tech$release <- Sys.info()["release"]
      private$..meta$tech$version <- Sys.info()["version"]

      invisible(self)
    }
  ),

  public = list(

    initialize = function(x, name = NULL) {
      private$setup(x, name)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Get Method                                  #
    #-------------------------------------------------------------------------#
    get = function(key = NULL, type = NULL) {

      if (is.null(type) & is.null(key)) return(private$..meta)

      if (!is.null(key)) {
        value <- private$search(key)
        if (is.null(value)) {
          message(paste0("Warning in class 'Meta', method 'get'.",
                         "Variable, ", key, " not found in metadata."))
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

      if (length(key) != length(value)) {
        event <- paste0("Error in 'Meta' class, 'set' method.  Key and value ",
                        "must be of equal lengths.")
        stop(event)
      }

      if (grepl("^d", type, ignore.case = TRUE)) {
        type <- 'descriptive'
      } else if (grepl("^f", type, ignore.case = TRUE)) {
        type <- 'functional'
      } else if (grepl("^q", type, ignore.case = TRUE)) {
        type <- 'quant'
      } else if ((grepl("^i", type, ignore.case = TRUE)) &
                 (key == 'name')) {
        type <- 'identity'
      } else {
        event <- paste0("Error in 'Meta' class, 'set' method. ",
                        "Unable to set", key ," as a(n) ", type, " variable. ",
                        "See?", class(self)[1], " for further assistance.")
        stop(event)
      }

      for (i in 1:length(key)) {
        private$..meta[[type]][[key[i]]] <-  value[[i]]
      }

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Admin Methods                               #
    #-------------------------------------------------------------------------#
    modified = function(event = NULL) {

      private$..meta$admin$modified <- Sys.time()
      private$..meta$admin$modifiedBy <- Sys.info()[["user"]]
      private$..meta$admin$nModified <- private$..meta$admin$nModified + 1
      private$..meta$admin$lastState <- ifelse(is.null(event), "Modified.", event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Query Methods                               #
    #-------------------------------------------------------------------------#
    query = function(key, value) {

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
