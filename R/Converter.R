#------------------------------------------------------------------------------#
#                                 Converter                                    #
#------------------------------------------------------------------------------#
#' Converter
#'
#' \code{Converter} Converts Corpus objects between native and foreign formats.
#'
#' @section Methods:
#'  \describe{
#'   \item{\code{new(...)}}{Instantiates the Converter object.}
#'   \item{\code{quanteda()}}{Converts to and from quanteda format.}
#'   \item{\code{tm()}}{Converts to and from tm format.}
#' }
#'
#' @param x Object to be converted
#' @return Converted object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter Classes
#' @export
Converter <- R6::R6Class(
  classname = "Converter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  public = list(

    initialize = function() { invisible(self) },

    quanteda = function(x) {
      return(ConverterQ$new()$convert(x))
    },

    tm = function(x) {
      return(ConverterTM$new()$convert(x))
    }
  )
)
