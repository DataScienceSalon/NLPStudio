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
      custom = list(),
      stats = list(),
      state = list()
    ),

    setIdentity = function(x, name = NULL, purpose = NULL) {

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
      private$..meta$identity$purpose <- purpose
      invisible(self)
    }
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this base class.") },

    #-------------------------------------------------------------------------#
    #                            Identity Methods                             #
    #-------------------------------------------------------------------------#
    getIdentity = function(key = NULL) {
      if (is.null(key)) {
        return(private$..meta$identity)
      } else if (!is.null(private$..meta$identity[[key]])) {
        return(private$..meta$identity[[key]])
      } else {
        event <- paste0("Variable, ", key, " is not a valid identity metadata ",
                        "variable.")
        private$logR$log(cls = private$..meta$identity$class, method = "getIdentity",
                         level = "Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Stats Methods                               #
    #-------------------------------------------------------------------------#
    getStats = function(key = NULL) {

      if (length(private$..meta$stats) == 0) {
        return(NULL)
      } else if (is.null(key)) {
        return(private$..meta$stats)
      } else if (!is.null(private$..meta$stats[[key]])) {
        return(private$..meta$stats[[key]])
      } else {
        event <- paste0("Key, '", key, "', is not a valid Stats metadata ",
                        "variable. See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log( method = 'getStats',
                         event = event, level = "Warn")
        invisible(self)
      }
    },

    setStats = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'setStats',
                         event = v$msg, level = "Error")
        stop()
      }

      for (i in 1:length(key)) {
        private$..meta$stats[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             State Methods                               #
    #-------------------------------------------------------------------------#
    getState = function() {
      return(private$..meta$state[c("current", "creator", "created",
                                    "modifier", "modified")])
      },

    modified = function(event = NULL) {

      private$..meta$state$current <- ifelse(is.null(event), "Modified.", event)
      private$..meta$state$modifier <- Sys.info()[["user"]]
      private$..meta$state$modified <- Sys.time()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Custom Metadata Methods                         #
    #-------------------------------------------------------------------------#
    getCustom = function(key = NULL) {

      if (length(private$..meta$custom) == 0) {
        return(NULL)
      } else if (is.null(key)) {
        return(private$..meta$custom)
      } else if (!is.null(private$..meta$custom[[key]])) {
        return(private$..meta$custom[[key]])
      } else {
        event <- paste0("Key, '", key, "', is not a valid custom metadata ",
                        "variable. See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'getCustom',
                          event = event, level = "Warn")
        return(FALSE)
      }
    },

    setCustom = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'setCustom',
                          event = v$msg, level = 'Error')
        stop()
      }

      for (i in 1:length(key)) {
        private$..meta$custom[[key[i]]] <- value[i]
      }
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
        for (j in 1:length(private$..meta)) {
          if (!is.null(private$..meta[[j]][[key[i]]])) {
            if (private$..meta[[j]][[key[i]]] %in% value[i])
              return(TRUE)
          }
        }
      }

      return(FALSE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      created <- format(private$..meta$state$created, format="%Y-%m-%d %H:%M:%S")
      s <- c(private$..meta$identity, private$..meta$custom, private$..meta$stats,
             creator = private$..meta$state$creator,
             created = created)
      s <- Filter(Negate(is.null), s)
      s <- as.data.frame(s, stringsAsFactors = FALSE, row.names = NULL)

      return(s)
    }
  )
)
