#==============================================================================#
#                               Document0                                      #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Component class defining base methods for all documents.
#'
#' This composite pattern component class defines members and methods common
#' across all the document related classes, including without limitation,
#' Corpus, TextDocument, TermFreq, POS, NGram, and Tokens objects.
#'
#' @section Document0 methods:
#'  \itemize{
#'   \item{\code{new()}}{Method not implemented for this component class.}
#'   \item{\code{attach(x)}}{Method for attaching objects.}
#'   \item{\code{detach(key, value)}}{Method for detaching objects.}
#'   \item{\code{attachments(key = NULL, value = NULL)}}{Lists an object's attachments.}
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
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..attachments = list(),

    #-------------------------------------------------------------------------#
    #                             Search Method                               #
    #-------------------------------------------------------------------------#
    # search
    #
    # This method enables users to query attachments using metadata. The method
    # performs a search based on the key/value pair parameters and returns
    # a boolean vector, indexing into the list of attachments, the element
    # or elements that match the search criteria.
    #
    search = function(cls, k, v) {

      listCondition <- rep(FALSE, length(private$..attachments[[cls]]))

      if (!is.null(private$..attachments[[cls]])) {
        if (length(k) == length(v)) {
          listCondition <- sapply(private$..attachments[[cls]], function(a) {
            found <- FALSE
            for (i in 1:length(k)) {
              if (a$query(cls = cls, key = k[i], value = v[i])) found <- TRUE
              return(found)
          }})
        } else if (length(k) == 1) {
          listCondition <- sapply(private$..attachments[[cls]], function(a) {
            if (a$query(cls = cls, key = k, value = v)) return(TRUE)
          })
        } else {
          event <- paste0("Invalid search criteria.  Key, must be of length ",
                          "one and/or length of the value vector.")
          private$logR$log(cls = class(self)[1], event = event, level = "Warn")
        }
      }

      return(listCondition)
    }
  ),

  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},


    #-------------------------------------------------------------------------#
    #                            Summary Methods                              #
    #-------------------------------------------------------------------------#
    attachments = function(quiet = FALSE) {
      attachmentz <- list()
      sections <- names(private$..attachments)

      if (!is.null(sections)) {
        for (i in 1:length(sections)) {
          attachmentz[[sections[i]]] <- rbindlist(lapply(private$..attachments[[sections[i]]], function(a) {
            as.data.frame(a$summary(abbreviated = TRUE))
          }))

          if (quiet == FALSE) {
            cat("\n\n", paste0(sections[i]), "\n")
            print(attachmentz[[sections[i]]], row.names = FALSE)
          }
        }
        names(attachmentz) <- sections
      }

      return(attachmentz)
    },

    #-------------------------------------------------------------------------#
    #                                 Attach                                  #
    #-------------------------------------------------------------------------#
    attach = function(x) {

      private$..params <- list()
      private$..params$x <- x
      if (private$validate("attach")$code == FALSE) stop()

      # Get document id and class, then attach.
      id <- x$getId()
      cls <- class(x)[1]
      attachment <- list()
      attachment[[id]] <- x

      if (!is.null(private$..attachments[[cls]])) {

        private$..attachments[[cls]] <- c(private$..attachments[[cls]], attachment)
      } else {
        private$..attachments[[cls]] <- list()
        private$..attachments[[cls]] <- c(private$..attachments[[cls]], attachment)
      }

      # Update date/time metadata and create log entry
      private$meta$modified()
      event <- paste0("Attached ", x$getName(), " object to ", self$getName(), ".")
      private$logR$log(cls = class(self)[1], event = event)

      return(self)

    },
    #-------------------------------------------------------------------------#
    #                                 Detach                                  #
    #-------------------------------------------------------------------------#
    detach = function(x) {

      id <- x$getId()
      cls <- class(x)[1]
      if (!is.null(private$..attachments[[cls]])) {
        if (!is.null(private$..attachments[[cls]][[id]])) {
          private$..attachments[[cls]][[id]] <- NULL
          private$meta$modified()
          event <- paste0("Detached ", x$getName(), " from ",
                                    self$getName(), ".")
          private$logR$log(cls = class(self)[1], event = event)
        } else {
          self$access()
          event <- paste0("Object is not attached to ",
                                    self$getName(), ". Returning NULL")
          private$logIt("Warn")
        }
      } else {
        self$access()
        event <- paste0("Object is not attached to ",
                                  self$getName(), ". Returning NULL")
        private$logIt("Warn")
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Get Attachment Method                           #
    #-------------------------------------------------------------------------#
    get = function(cls, key = NULL, value = NULL) {

      # If parameters are null, return all attachments, otherwise search.
      objects <- NULL
      if (is.null(key)) {
        if (!is.null(private$..attachments[[cls]])) {
          objects <- private$..attachments[[cls]]
        }
      } else {
        listCondition <- private$search(cls, key, value)
        if (!is.null(listCondition)) {
          objects <- private$..attachments[[cls]][listCondition]
        }
      }

      private$meta$accessed()

      return(objects)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(core = TRUE, state = TRUE, system = TRUE,
                       attachments = TRUE, quiet = FALSE, abbreviated = FALSE) {

      meta <- private$meta$get()

      if (abbreviated) {
        result <- private$oneLiner(meta = meta, quiet = quiet)
      } else {
        result <- list()
        section <- character()

        if (core) {
          result$meta <- private$core(meta,  quiet = quiet)
          section <- c("Additional Core Metadata")
        }

        if (attachments) {
          result$attachments <- private$attachments(meta, quiet = quiet)
          section <- c(section, "Attachments")
        }

        if (state) {
          result$state <- private$state(meta, quiet = quiet)
          section <- c(section, "State Information")
        }

        if (system) {
          result$sys <- private$system(meta, quiet = quiet)
          section <- c(section, "System Information")
        }

        names(result) <- section
      }
      invisible(result)
    }
  )
)
