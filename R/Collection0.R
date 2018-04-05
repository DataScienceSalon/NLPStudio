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
          a$meta$query(key = key, value = value)
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

      listCondition <- private$search(key, value)
      return(private$..documents[[listCondition]])
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
      private$attachments[[credentials$id]] <- x
      credentials <- as.data.frame(credentials)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      event <- paste0("Attached ", x$getName(), " object to ", self$getName(), ".")
      private$meta$modified(event = event)
      private$logR$log( method = 'addDocument',
                       event = event)

      return(self)

    },
    #-------------------------------------------------------------------------#
    #                             removeDocument                              #
    #-------------------------------------------------------------------------#
    removeDocument = function(x) {

      id <- x$getId()

      if (!is.null(private$..documents[[id]])) {
        private$..documents[[id]] <- NULL
        private$..inventory <- subset(private$..inventory, id != id)
        event <- paste0("Removed ", x$getName(), " from ",
                                  self$getName(), ".")
        private$meta$modified(event = event)
        private$logR$log( method = 'removeDocument',
                         event = event)
      } else {
        self$access()
        event <- paste0("Object is not attached to ",
                        self$getName(), ". ")
        private$logR$log( method = 'removeDocument',
                         event = event, level = "Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Summary Methods                              #
    #-------------------------------------------------------------------------#
    summarizeDocuments = function(verbose = TRUE) {

      summaries <- list()

      families <- unique(private$..inventory$family)

      for (i in 1:length(families)) {
        documents <- subset(private$..inventory, family == families[i])
        cat(paste0("\n\n", families[i], ":\n"))
        for (j in 1:nrow(documents)) {
          id <- documents$id[j]
          document <- self$getDocument(key = 'id', value = id)
          summaries[[id]] <- document$summary(abbreviated = TRUE, verbose)
          colnames(summaries[[id]]) <- sapply(colnames(summaries[[id]]),
                                              function(x) {proper(x)})
          print(summaries[[id]], row.names = FALSE)
        }
      }
      return(summaries)
    }
  )
)
