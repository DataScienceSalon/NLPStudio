#------------------------------------------------------------------------------#
#                               Collection0                                    #
#------------------------------------------------------------------------------#
#' Collection0
#'
#' \code{Collection0} Abstract class for all Collection classes
#'
#' This abstract class defines members and methods common to the Corpus and
#' TextDocument collection / composite objects.
#'
#' @section Collection0 methods:
#'  \itemize{
#'   \item{\code{new(name = NULL, purpose = NULL)}}{Not implemented.}
#'   \item{\code{getDocument(key, value)}}{Returns the document or documents that
#'   match the key/value pair(s). }
#'   \item{\code{addDocument(x)}}{Adds a document to the Collection object.}
#'   \item{\code{removeDocument(x)}}{Removes a document from the Collection
#'   object.}
#'   \item{\code{summary(x)}}{Summarizes a Collection document.}
#'  }
#'
#'  @param x Object or list of objects to be attached
#'  @param key Character string or vector of strings indicating the metadata
#'  variable or variables used for matching
#'  @param value Character string or vector of strings indicating the
#'  metadata value associated with the key(s) parameter.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Collection0 <- R6::R6Class(
  classname = "Collection0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..documents = list(),
    ..inventory = data.frame(),

    #-------------------------------------------------------------------------#
    #                             Search Method                               #
    #-------------------------------------------------------------------------#
    search = function(key, value) {

      listCondition <- rep(FALSE, length(private$..documents))

      if (!is.null(private$..documents)) {
        listCondition <- sapply(private$..documents, function(a) {
          a$query(key = key, value = value)
        })
      }

      return(listCondition)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             getDocument                                 #
    #-------------------------------------------------------------------------#
    getDocument = function(key, value) {

      if (is.null(key)) return(private$..documents)

      # Validate key/value pair
      private$..params <- list()
      private$..params$kv$key <- key
      private$..params$kv$value <- value
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'getDocument',
                         event = v$msg, level = "Error")
        stop()
      }

      # Search for documents that match the metadata and return
      listCondition <- private$search(key, value)
      result <- private$..documents[listCondition]
      if (length(result) == 1) result <- result[[1]]

      return(result)
    },

    #-------------------------------------------------------------------------#
    #                             addDocument                                 #
    #-------------------------------------------------------------------------#
    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Document0')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }

      # Get document credentials, add document and update inventory
      credentials <- x$getIdentity()
      private$..documents[[credentials$id]] <- x
      credentials <- as.data.frame(credentials, stringsAsFactors = FALSE, row.names = NULL)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      event <- paste0("Attached ", x$getName(), " object to ", self$getName(), ".")
      private$meta$modified(event = event)
      private$logR$log(method = 'addDocument',
                       event = event)

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                             removeDocument                              #
    #-------------------------------------------------------------------------#
    removeDocument = function(x) {

      identifier <- x$getId()

      if (!is.null(private$..documents[[identifier]])) {
        private$..documents[[identifier]] <- NULL
        private$..inventory <- private$..inventory %>%
          dplyr::filter(id != identifier)
        event <- paste0("Removed ", x$getName(), " from ",
                                  self$getName(), ".")
        private$meta$modified(event = event)
        private$logR$log( method = 'removeDocument',
                         event = event)
      } else {
        event <- paste0("Object is not attached to ",
                        self$getName(), ". ")
        private$logR$log( method = 'removeDocument',
                         event = event, level = "Warn")
      }
      invisible(self)
    }
  )
)
