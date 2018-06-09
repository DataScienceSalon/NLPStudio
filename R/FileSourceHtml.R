#------------------------------------------------------------------------------#
#                             FileSourceHTML                                   #
#------------------------------------------------------------------------------#
#' FileSourceHTML
#'
#' \code{FileSourceHTML} Class creates a File object from .HTML file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceHTML <- R6::R6Class(
  classname = "FileSourceHTML",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileSource0,

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(path) {
      private$loadServices()
      private$..path <- path
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
