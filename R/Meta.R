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

    initialize = function(x, name = NULL) {

      private$loadDependencies()
      private$setup(x, name)
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
