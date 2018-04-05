#------------------------------------------------------------------------------#
#                               Document0                                      #
#------------------------------------------------------------------------------#
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
#'   \item{\code{summary()}}{Prints a summary of a Document family class object.}
#'  }
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

  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},

    #-------------------------------------------------------------------------#
    #                         Convenience Getters                             #
    #-------------------------------------------------------------------------#
    getId = function() private$meta$getIdentity(key = 'id'),
    getName = function() private$meta$getIdentity(key = 'name'),
    getIdentity = function() private$meta$getIdentity(),

    #-------------------------------------------------------------------------#
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    metadata = function(key, value) {
      private$meta$setCustom(key, value)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeId = function(verbose = TRUE) {

      identity <- private$meta$getIdentity()
      if (verbose) {
        cat(paste0("\n\nObject Family: ", identity$family))
        cat(paste0("\nObject Class : ", identity$class))
        cat(paste0("\nObject Id    : ", identity$id))
        cat(paste0("\nObject Name  : ", identity$name))
        cat(paste0("\nPurpose      : ", identity$purpose))
      }
      return(identity)

    },

    summarizeCustomMeta = function(verbose = TRUE) {
      meta <- private$meta$getCustom()
      if (verbose) {
        if (!is.null(meta)) {
          metaDf <- as.data.frame(meta, stringsAsFactors = FALSE, row.names = NULL)
          colnames(metaDf) <- sapply(colnames(metaDf), function(x) {proper(x)})
          cat("\n\nMetadata:\n")
          print(metaDf, row.names = FALSE)
        }
      }
      return(meta)
    },

    summarizeStats = function(verbose = TRUE) {
      stats <- private$meta$getStats()
      if (verbose) {
        if (!is.null(stats)) {
          statsDf <- as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL)
          colnames(statsDf) <- sapply(colnames(statsDf), function(x) {proper(x)})
          cat("\n\nStatistics:\n")
          print(statsDf, row.names = FALSE)
        }
      }
      return(stats)
    },

    summarizeState = function(verbose = TRUE) {
      state <- private$meta$getState()
      if (verbose) {
        stateDf <- as.data.frame(state, stringsAsFactors = FALSE, row.names = NULL)
        colnames(stateDf) <- sapply(colnames(stateDf), function(x) {proper(x)})
        cat("\nState:\n")
        print(stateDf, row.names = FALSE)
      }
      return(state)
    },

    summary = function(verbose = TRUE, abbreviated = FALSE) {

      if (abbreviated == FALSE) {
        sd <- list()
        sd$id <- self$summarizeId(verbose)
        sd$stats <- self$summarizeStats(verbose)
        sd$meta  <- self$summarizeCustomMeta(verbose)
        if (length(private$..documents) > 0) {
          sd$documents <- self$summarizeDocuments(verbose)
        }
        sd$state <- self$summarizeState(verbose)
        invisible(sd)
      } else {
        invisible(private$meta$summary(verbose))
      }
    },

    #-------------------------------------------------------------------------#
    #                            Receive Method                               #
    #-------------------------------------------------------------------------#
    receive = function(x, notice) {
      private$logR$log(x = x, event = notice, level = "Info")
    },

    #-------------------------------------------------------------------------#
    #                           Print Log Method                              #
    #-------------------------------------------------------------------------#
    printLog = function() {
      invisible(private$logR$printLog())
    }
  )
)
