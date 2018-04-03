#' Meta
#'
#' \code{Meta} Base class for metadata objects.
#'
#' Contains object metadata and the methods for adding, removing, and reporting
#' metadata.  Also includes convenience methods for updating state metadata
#' relating to datetimes of creating, update, and access.
#'
#' @section Meta methods:
#'  \itemize{
#'   \item{\code{new()}}{Intantiates an instance of the Meta class.}
#'   \item{\code{application(key, value)}}{Adds metadata via key value pairs.}
#'   \item{\code{stats(key)}}{Returns metadata associated with the key parameter.}
#'   \item{\code{created()}}{Sets state datetime parameters when an object is created.}
#'   \item{\code{modified()}}{Sets state datetime parameters when an object is modified.}
#'   \item{\code{accessed()}}{Sets state datetime parameters when an object is accessed.}
#'  }
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
    ..application = list(),
    ..document = list(),
    ..stats = list(),
    ..state = list()
  ),

  public = list(

    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },

    getId = function() private$..application$id,
    getName = function() private$..application$name,

    document = function(key, value) {

      for (i in 1:length(key)) {
        private$..document[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    setStats = function(key, value) {

      for (i in 1:length(key)) {
        private$..stats[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    get = function(key = NULL) {

      if (is.null(key)) {
        metaDataList <- Filter(Negate(is.null), list(private$..core,
                                                     private$..stats,
                                                     private$..state,
                                                     private$..system))
        metaDataDfs <- lapply(metaDataList, function(m) {
          df <- as.data.frame(m, stringsAsFactors = FALSE)
          if (nrow(df) > 0 ) {
            df
          }
        })
        names(metaDataDfs) <- c("core", "stats", "state", "system")
        metaDataDfs <- Filter(Negate(is.null), metaDataDfs)
        return(metaDataDfs)

      } else if (!is.null(key)) {

        # Check object level metadata
        keys <- intersect(names(private$..core), key)
        if (length(keys) > 0) {
          return(as.data.frame(private$..core[names(private$..core) %in% keys],
                               stringsAsFactors = FALSE))
        } else {

          # Check stats meta data
          keys <- intersect(names(private$..stats), key)
          if (length(keys) > 0) {
            return(as.data.frame(private$..stats[names(private$..stats) %in% keys],
                                 stringsAsFactors = FALSE))
          } else {

            # Check state level metadata
            keys <- intersect(names(private$..state), key)
            if (length(keys) > 0) {
              return(as.data.frame(private$..state[names(private$..state) %in% keys],
                                   stringsAsFactors = FALSE))
            }
          }
        }
      }
    },

    getName = function() { private$..core$name },
    getId = function() { private$..core$id},

    created = function(family, cls, id, name) {

      # Format object metadata
      private$..object$family <- family
      private$..object$class <- cls
      private$..object$name <- name
      private$..object$id <- id

      # Create state metadata
      private$..state$createdBy <- Sys.info()[['user']]
      private$..state$modifiedBy <- Sys.info()[["user"]]
      private$..state$accessedBy <- Sys.info()[["user"]]
      private$..state$current <- paste0("Instantiated.")
      private$..state$created <- Sys.time()
      private$..state$modified <- Sys.time()
      private$..state$accessed <- Sys.time()

      invisible(self)
    },

    modified = function(event = NULL) {

      private$..state$modifiedBy <- Sys.info()[["user"]]
      private$..state$current <- ifelse(is.null(event), "Modified.", event)
      private$..state$modified <- Sys.time()
      private$..state$accessed <- Sys.time()

      invisible(self)
    },

    accessed = function() {
      private$..state$accessedBy <- Sys.info()[["user"]]
      private$..state$accessed <- Sys.time()
      invisible(self)
    }
  )
)
