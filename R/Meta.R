#' Meta
#'
#' \code{Meta} Generic or default Metadata class.
#'
#' @template metadataDescription
#'
#' @section Meta Methods:
#' @template metadataMethods
#'
#' @template metadataParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta0,

  public = list(

    initialize = function(x, name = NULL, purpose = NULL) {

      private$loadDependencies()
      private$setIdentity(x, name, purpose)
      private$..meta$state$current <- paste0("Instantiated.")
      private$..meta$state$creator <- Sys.info()[['user']]
      private$..meta$state$created <- Sys.time()
      private$..meta$state$modifier <- Sys.info()[['user']]
      private$..meta$state$modified <- Sys.time()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$meta(self)
    }
  )
)
