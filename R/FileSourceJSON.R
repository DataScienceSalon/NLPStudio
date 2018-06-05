#------------------------------------------------------------------------------#
#                             FileSourceJSON                                   #
#------------------------------------------------------------------------------#
#' FileSourceJSON
#'
#' \code{FileSourceTxt} Class creates a File object from .JSON file.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSourceJSON <- R6::R6Class(
  classname = "FileSourceJSON",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileSource0,

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This class is not implemented.") },

    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    source = function(path) { stop("This class is not implemented.") },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
