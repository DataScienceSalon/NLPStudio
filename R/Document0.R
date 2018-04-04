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
#' @family Document Family
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
    summarizeId = function() {

      identity <- meta$getIdentity()
      cat(paste0("\n\nObject Family: ", identity$family))
      cat(paste0("\nObject Class : ", identity$class))
      cat(paste0("\nObject Id    : ", identity$id))
      cat(paste0("\nObject Name  : ", identity$name))
      cat(paste0("\nPurpose      : ", identity$purpose))

    },

    summarizeStats = function() {
      stats <- meta$getStats()
      if (!is.null(stats)) {
        stats <- as.data.frame(stats, stringsAsFactors = FALSE, row.names = NULL)
        colnames(stats) <- sapply(colnames(stats), function(x) {proper(x)})
        cat("\n\nStatistics\n")
        print(stats, row.names = FALSE)
      }
    },

    summarizeState = function() {
      state <- meta$getState()
      state <- c(Current = state$current, state[-2])
      state <- as.data.frame(state, stringsAsFactors = FALSE, row.names = NULL)
      colnames(state) <- sapply(colnames(state), function(x) {proper(x)})
      cat("\nState\n")
      print(state, row.names = FALSE)
    }
  ),

  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},

    #-------------------------------------------------------------------------#
    #                            Summary Methods                              #
    #-------------------------------------------------------------------------#
    summary = function() {

      private$summarizeId()
      private$summarizeStats()
      private$summarizeState()

      invisible(self)
    }
  )
)
