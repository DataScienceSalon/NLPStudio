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

  private = list(
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

      if (length(meta) > 0) {
        if (verbose) {
          metaDf <- as.data.frame(meta, stringsAsFactors = FALSE, row.names = NULL)
          cat("\n\nMetadata:\n")
          print(metaDf, row.names = FALSE)
        }
        return(meta)
      }
      return(NULL)
    },

    summarizeStats = function(verbose = TRUE) {
      stats <- private$meta$getStats()
      if (verbose) {
        if (!is.null(stats)) {
          statsDf <- as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL)
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
        cat("\nState:\n")
        print(stateDf, row.names = FALSE)
        cat("\n")
      }
      return(state)
    },

    summarizeDocuments = function(verbose = TRUE) {

      summaries <- list()
      meta <- list()
      familySummary <- data.frame()

      families <- unique(private$..inventory$family)

      for (i in 1:length(families)) {
        documents <- subset(private$..inventory, family == families[i])

        # Get metadata for each document
        for (j in 1:nrow(documents)) {
          id <- documents$id[j]
          document <- private$..documents[[id]]
          meta[[id]] <- document$metadata()
        }

        # Extract identity information
        identity <- rbindlist(lapply(meta, function(m) {
          m$identity
        }))

        # Extract stats
        stats <- rbindlist(lapply(meta, function(m) {
          m$stats
        }))

        # Extract custom metadata
        custom <- rbindlist(lapply(meta, function(m) {
          m$custom
        }), fill = TRUE, use.names = TRUE)

        # # Extract state information
        # state <- rbindlist(lapply(meta, function(m) {
        #   m$state
        # }))

        # Combine and format columns
        if (nrow(custom) > 0) {
          familySummary <- cbind(identity, custom, stats)
        } else {
          familySummary <- cbind(identity, stats)
        }

        familySummary[is.na(familySummary)] <- " "

        # Print Results if verbose
        if (verbose) {
          cat(paste0("\n\n", families[i], ":\n"))
          print(familySummary[,-1], row.names = FALSE)
        }

        summaries <- c(summaries, familySummary)
      }
      return(summaries)
    }
  ),



  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},

    #-------------------------------------------------------------------------#
    #                 Convenience Getters and Query Method                    #
    #-------------------------------------------------------------------------#
    getId = function() private$meta$getIdentity(key = 'id'),
    getName = function() private$meta$getIdentity(key = 'name'),
    getIdentity = function() private$meta$getIdentity(),
    query = function(key, value) private$meta$query(key, value),

    #-------------------------------------------------------------------------#
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    metadata = function(key = NULL, value = NULL) {
      if (is.null(key)) {
        return(private$meta$getMeta())
      } else {
        private$meta$setCustom(key, value)
        invisible(self)
      }
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summary = function(id = TRUE, stats = TRUE, custom = TRUE,
                       documents = TRUE, state = TRUE, verbose = TRUE,
                       abbreviated = FALSE) {

      if (abbreviated == FALSE) {
        sd <- list()
        if (id) sd$id <- private$summarizeId(verbose)
        if (stats) sd$stats <- private$summarizeStats(verbose)
        if (custom) sd$meta  <- private$summarizeCustomMeta(verbose)
        if (documents) {
          if (length(private$..documents) > 0) {
            sd$documents <- private$summarizeDocuments(verbose)
          }
        }
        if (state) sd$state <- private$summarizeState(verbose)
        invisible(sd)
      } else {
        invisible(private$meta$summary())
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
