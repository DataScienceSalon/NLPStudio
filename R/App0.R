#==============================================================================#
#                                    App0                                      #
#==============================================================================#
#' App0
#'
#' \code{App0} Abstract class  for the App family of classes.
#'
#' This abstract class from which all *App classes are derived.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family App classes
#' @export
App0 <- R6::R6Class(
  classname = "App0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$app(self)
    }
  )
)
