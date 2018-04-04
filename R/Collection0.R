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
#' @family Collection Classes
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
    },

    #-------------------------------------------------------------------------#
    #                     Summarize Documents Method                          #
    #-------------------------------------------------------------------------#
    summarizeDocuments = function() {

      families <- unique(private$..inventory$family)

      for (i in 1:length(families)) {
        documents <- subset(private$..inventory, family == families[i])
        cat(paste0("\n\n", families[i], "\n"))
        for (j in 1:nrow(documents)) {
          document <- self$getDocument(key = 'id', value = documents$id[j])
          summary <- as.data.frame(document$meta$summary(),
                                   stringsAsFactors = FALSE, row.names = NULL)
          colnames(summary) <- sapply(colnames(summary), function(x) {proper(x)})
          print(summary, row.names = FALSE)
        }
      }
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
        private$logR$log(cls = class(self)[1], method = 'getDocument',
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
      private$..params$classes$objects <- x
      private$..params$classes$valid <- 'Document0'
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(cls = class(self)[1], method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }

      # Get document credentials, add document and update inventory
      credentials <- x$getIdentity()
      private$attachments[[credentials$id]] <- x
      credentials <- as.data.frame(credentials)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      private$meta$modified()
      event <- paste0("Attached ", x$getName(), " object to ", self$getName(), ".")
      private$logR$log(cls = class(self)[1], method = 'addDocument',
                       event = event, level = "info")

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
        private$meta$modified()
        event <- paste0("Removed ", x$getName(), " from ",
                                  self$getName(), ".")
        private$logR$log(cls = class(self)[1], method = 'removeDocument',
                         event = event, level = "info")
      } else {
        self$access()
        event <- paste0("Object is not attached to ",
                                  self$getName(), ". Returning NULL")
        private$logIt("Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Summary Methods                              #
    #-------------------------------------------------------------------------#
    summary = function() {

      private$summarizeId()
      private$summarizeStats()
      private$summarizeState()
      private$summarizeDocuments()

      invisible(self)
    }
  )
)
