#' MetaText
#'
#' \code{MetaText} Text class containing the methods for creating, managing and reporting metadata for Corpus and TextDocument objects.
#'
#' @template metadataDescription.R
#'
#' @section Meta methods:
#' @template metadataMethods.R
#'
#' @template metadataParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
MetaText <- R6::R6Class(
  classname = "MetaText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta,

  private = list(
    stats = list()
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
    #                             Stats Methods                               #
    #-------------------------------------------------------------------------#
    getStats = function() { private$..stats },

    setStats = function(key, value) {

      if (length(key) != length(value)) {
        event <- paste0("Key and value must be of equal length.")
        private$logR$log(cls = class(self)[1], method = 'setStats',
                         level = "Error")
        stop()
      }

      for (i in 1:length(key)) {
        private$..stats[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      s <- c(private$..identity, private$..stats,
             created = private$..state$created,
             creator = private$..state$creator)

      return(s)
    }
  )
)

