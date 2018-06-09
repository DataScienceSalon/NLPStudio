#------------------------------------------------------------------------------#
#                             FileSourceCsv                                    #
#------------------------------------------------------------------------------#
#' FileSourceCsv
#'
#' \code{FileSourceCsv} Class creates a File object from .Csv file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceCsv <- R6::R6Class(
  classname = "FileSourceCsv",
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
