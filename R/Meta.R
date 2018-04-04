#' Meta
#'
#' \code{Meta} Base class for metadata objects.
#'
#' @template metadataDescription
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
    ..identity = list(),
    ..custom = NULL,
    ..stats = NULL,
    ..state = list()
  ),

  public = list(

    initialize = function() {

      private$loadDependencies()
      private$..state$creator <- Sys.info()[['user']]
      private$..state$current <- paste0("Instantiated.")
      private$..state$created <- Sys.time()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                            Identity Methods                             #
    #-------------------------------------------------------------------------#
    getIdentity = function(key = NULL) {
      if (is.null(key)) {
        return(private$..identity)
      } else if (!is.null(private$..identity[[key]])) {
        return(private$..identity[[key]])
      } else {
        event <- paste0("Variable, ", key, " is not a valid identity metadata ",
                        "variable.")
        private$logR$log(cls = private$..identity$class, method = "getIdentity",
                         level = "Warn")
      }
      invisible(self)
    },

    setIdentity = function(x, name = NULL, purpose = NULL) {

      # Designate family, class and purpose
      private$..identity$family <- class(x)[2]
      private$..identity$class <- class(x)[1]

      # Creates unique identifier
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      private$..identity$id <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                                  sample(1:1000, 1, replace = TRUE), settings)

      # Designate/create object name
      private$..identity$name <- ifelse(is.null(name),
                                        paste0(private$..identity$class,
                                               " (", toupper(private$..identity$id),
                                               ")"), name)
      private$..identity$purpose <- purpose
      invisible(self)
    },

    checkIdentity = function(family = NULL, cls = NULL, name = NULL, id = NULL,
                             purpose = NULL) {

      params <- c(family, cls, name, id, purpose)

      if (sum(!is.null(params)) == 0) return(FALSE)
      if (!is.null(family) & (sum(private$..identity$family %in% family) == 0)) return(FALSE)
      if (!is.null(cls) & (sum(private$..identity$class %in% cls) == 0)) return(FALSE)
      if (!is.null(name) & (sum(private$..identity$name %in% name) == 0)) return(FALSE)
      if (!is.null(id) & (sum(private$..identity$id %in% id) == 0)) return(FALSE)
      if (!is.null(purpose) & (sum(private$..identity$purpose %in% purpose) == 0)) return(FALSE)
      return(TRUE)

    },

    #-------------------------------------------------------------------------#
    #                         Custom Metadata Methods                         #
    #-------------------------------------------------------------------------#
    getCustom = function() { private$..custom },

    setCustom = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) stop()

      for (i in 1:length(key)) {
        private$..custom[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Stats Methods                               #
    #-------------------------------------------------------------------------#
    getStats = function() { private$..stats },

    setStats = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) stop()

      for (i in 1:length(key)) {
        private$..stats[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             State Methods                               #
    #-------------------------------------------------------------------------#
    getState = function() { return(private$..state) },

    modified = function(event = NULL) {

      private$..state$modifier <- Sys.info()[["user"]]
      private$..state$current <- ifelse(is.null(event), "Modified.", event)
      private$..state$modified <- Sys.time()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      s <- c(private$..identity, private$..custom, private$..stats,
             creator = private$..state$creator,
             created = private$..state$created)
      s <- Filter(Negate(is.null), s)

      return(s)
    }
  )
)
