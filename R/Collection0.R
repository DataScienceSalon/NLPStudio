#------------------------------------------------------------------------------#
#                               Collection0                                    #
#------------------------------------------------------------------------------#
#' Collection0
#'
#' \code{Collection0} Abstract class for all Collection classes
#'
#' This abstract class defines members and methods common to the Corpus and
#' Document collection / composite objects.
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
    },

    #-------------------------------------------------------------------------#
    #                             attach Document                             #
    #-------------------------------------------------------------------------#
    attach = function(x) {
    # Get document credentials, add document and update inventory
      credentials <- x$getMeta(type = 'identity')
      private$..documents[[credentials$id]] <- x
      credentials <- as.data.frame(credentials, stringsAsFactors = FALSE, row.names = NULL)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      event <- paste0("Added ", x$getName(), " object to ", self$getName(), ".")
      private$meta$modified(event = event)
      private$logR$log(method = 'addDocument', event = event)
    },

    #-------------------------------------------------------------------------#
    #                     Summary Documents Method                            #
    #-------------------------------------------------------------------------#
    summarizeDocMeta = function(classname = NULL) {

      if (length(private$..documents) == 0) return(NULL)

      heading <- paste0("\n\nDocuments attached to ", self$getId(), ":")
      cat(heading)
      documentMeta <- self$getDocMeta(classname)
      classes <- names(documentMeta)
      lapply(seq_along(documentMeta), function(x) {

        # Combine relevant metadata types
        sections <- list(documentMeta[[x]]$identity,
                         documentMeta[[x]]$descriptive,
                         documentMeta[[x]]$quant,
                         documentMeta[[x]]$functional,
                         documentMeta[[x]]$admin,
                         documentMeta[[x]]$tech)

        # Combine data into a single data frame
        docSummary <- sections[[1]]
        for (i in 2:length(sections)) {
          if (nrow(sections[[i]]) > 0)  docSummary <- cbind(docSummary, sections[[i]])
        }

        # Remove non-essential data, replace NAs with spaces and print
        docSummary <- docSummary %>% select(-modified, -modifiedBy, -nModified,
                                            -lastState, -hardware, -os,
                                            -release, -version)
        docSummary[is.na(docSummary)] <- " "
        cat("\n")
        print(docSummary, row.names = FALSE)
      })

      return(documentMeta)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             getDocument                                 #
    #-------------------------------------------------------------------------#
    getDocument = function(key = NULL, value = NULL) {

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

      private$attach(x)

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
        private$logR$log(method = 'removeDocument',
                         event = event)
      } else {
        event <- paste0("Object is not attached to ",
                        self$getName(), ". ")
        private$logR$log( method = 'removeDocument',
                         event = event, level = "Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Document Metadata                             #
    #-------------------------------------------------------------------------#
    getDocMeta = function(classname = NULL, type = NULL) {

      if (nrow(private$..inventory) == 0) return(NULL)

      classes <- unique(private$..inventory$classname)

      if (!is.null(classname)) {
        if (classname %in% classes) {
          classes <- classname
        } else {
          event <- paste0("No documents of the ", classname, " class are ",
                          "attached to this object.")
          private$logR$log(x = self, event = event, method = "getDocMeta",
                           level = "Error")
          stop()
        }
      }

      documentMeta <- list()

      for (i in 1:length(classes)) {
        documents <- subset(private$..inventory, classname == classes[i])

        # Get metadata for each document
        meta <- list()
        for (j in 1:nrow(documents)) {
          id <- documents$id[j]
          document <- private$..documents[[id]]
          meta[[id]] <- document$getMeta(type = type)
        }

        type <- list()

        # Extract identity information
        type$identity <- rbindlist(lapply(meta, function(m) {
          m$identity
        }))

        # Extract descriptive metadata
        type$descriptive <- as.data.frame(rbindlist(lapply(meta, function(m) {
          m$descriptive
        }), fill = TRUE, use.names = TRUE))
        type$descriptive[is.na(type$descriptive)] <- " "

        # Extract functional metadata
        type$functional <- as.data.frame(rbindlist(lapply(meta, function(m) {
          m$functional
        }), fill = TRUE, use.names = TRUE))
        type$functional[is.na(type$functional)] <- " "

        # Extract quant metadata
        type$quant <- rbindlist(lapply(meta, function(m) {
          m$quant
        }))

        # Extract admin metadata
        type$admin <- rbindlist(lapply(meta, function(m) {
          m$admin
        }))

        # Extract technical metadata
        type$tech <- rbindlist(lapply(meta, function(m) {
          m$tech
        }))

        documentMeta[[classes[i]]] <- type
      }
      return(documentMeta)
    },
    #-------------------------------------------------------------------------#
    setDocMeta = function(docMeta, classname = "Document",
                          type = 'descriptive') {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('docMeta')
      private$..params$classes$objects <- list(docMeta)
      private$..params$classes$valid <- list(c('data.frame', 'data.table'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'setDocMeta',
                         event = v$msg, level = "Error")
        stop()
      }

      cls <- classname
      if (nrow(private$..inventory %>% filter(classname == cls)) == 0) {
        event <- paste0("There are no documents of type ", classname,
                        " attached to this collection. See?",
                        class(self)[1], " for further assistance.")
        private$logR$log(method = 'setDocMeta', event = event,
                         level = "Error")
        stop()
      }

      docs <- self$getDocument(key = 'classname', value = classname)

      if (nrow(docMeta) != length(docs)) {
        event <- paste0("The docMeta variable must be a data.frame or ",
                        "data.table with one row for each document in ",
                        "the collection. See?", class(self)[1], " for ",
                        "further assistance.")
        private$logR$log(method = "setDocMeta", event = event, level = "Error")
      }

      vars <- names(docMeta)
      for (i in 1:nrow(docMeta)) {
        for (j in 1:length(docMeta))
          docs[[i]]$setMeta(key = vars[j], value = docMeta[i,j], type = type)
      }

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(select = NULL) {

      private$..params <- list()
      private$..params$discrete$variables <- 'select'
      private$..params$discrete$values <- select
      private$..params$discrete$valid <- list(c('id', 'descriptive', 'functional',
                                                'quant', 'documents', 'admin',
                                                'tech'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'summary',  event = v$msg, level = "Error")
        stop()
      }

      sd <- list()

      if (is.null(select)) {
        sd$id <- private$summarizeIdMeta()
        sd$descriptive <- private$summarizeDescriptiveMeta()
        sd$quant <- private$summarizeQuantMeta()
        sd$functional  <- private$summarizeFunctionalMeta()
        sd$documents <- private$summarizeDocMeta()
        sd$admin <- private$summarizeAdminMeta()
        sd$tech <- private$summarizeTechMeta()
        invisible(sd)
      } else {
        sd <- switch(select,
                     id = private$summarizeIdMeta(),
                     descriptive = private$summarizeDescriptiveMeta(),
                     functional = private$summarizeFunctionalMeta(),
                     documents <- private$summarizeDocMeta(),
                     quant = private$summarizeQuantMeta(),
                     admin = private$summarizeAdminMeta(),
                     tech = private$summarizeTechMeta()
        )
      }

      invisible(sd)
    }
  )
)
