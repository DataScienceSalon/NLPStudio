#==============================================================================#
#                                    Cmd0                                      #
#==============================================================================#
#' Cmd0
#'
#' \code{Cmd0} Abstract class  for the App family of classes.
#'
#' This abstract class from which all *App classes are derived.
#'
#' @param x Receiver class object
#' @param target Target class object
#' @param ... Additional parameters
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family App classes
#' @export
Cmd0 <- R6::R6Class(
  classname = "Cmd0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },
    execute = function(target) { stop("Not implemented for this abstract/interface class.") },
    getInputClass = function() { stop("Not implemented for this abstract/interface class.") },
    getOutputClass = function() { stop("Not implemented for this abstract/interface class.") },
    getReceiver = function()  { stop("Not implemented for this abstract/interface class.") },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cmd(self)
    }
  )
)
