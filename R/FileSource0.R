#------------------------------------------------------------------------------#
#                                    FileSource0                               #
#------------------------------------------------------------------------------#
#' FileSource0
#'
#' \code{FileSource0}  Abstract class for the FileSource family of classes.
#'
#' Abstract strategy class which defines the methods common to the FileSource
#' family of classes.
#'
#' @template fileSourceParams
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileSource Family of Classes
#' @export
FileSource0 <- R6::R6Class(
  classname = "FileSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(
    ..origin = character(),
    ..destination = character()
  ),

  public = list(
    initialize = function() { stop(paste("This method is not implemented for ",
                                         "this abstract class.")) },
    getOrigin = function() private$..origin,
    getDestination = function() private$..destination,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileSource(self)
    }
  )
)
