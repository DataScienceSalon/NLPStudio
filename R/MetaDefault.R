#' MetaDefault
#'
#' \code{MetaDefault} Default class containing the methods for creating, managing and reporting metadata.
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
MetaDefault <- R6::R6Class(
  classname = "MetaDefault",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta,

  public = list(

    initialize = function() {

      private$loadDependencies()
      private$..state$creator <- Sys.info()[['user']]
      private$..state$current <- paste0("Instantiated.")
      private$..state$created <- Sys.time()
      invisible(self)

    }
  )
)

