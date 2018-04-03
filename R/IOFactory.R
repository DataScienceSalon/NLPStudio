#==============================================================================#
#                                   IOFactory                                  #
#==============================================================================#
#' IOFactory
#'
#' \code{IOFactory} Factory class that creates the appropriate IO class.
#'
#' Class creates IO objects based upon the file type
#'
#' @section IOFactory methods:
#' \describe{
#'  \item{\code{new()}}{Instantiates the factory.}
#'  \item{\code{getFileStrategy(fileName)}}{Instantiates and returns the appropriate File Strategy object.}
#' }
#'
#' @param path Character string containing the path for file
#'
#' @return Concrete IO strategy.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family IO Classes
#' @export
IOFactory <- R6::R6Class(
  "IOFactory",
  lock_class = FALSE,
  lock_objects = FALSE,

  private = list(
    ..path = character(),
    logR = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                      Object Creation and Read                           #
    #-------------------------------------------------------------------------#
    initialize = function(path) {
      private$logR <- LogR$new()
      private$..path <- path
      invisible(self)
    },

    getIOStrategy = function() {

      type <- tolower(tools::file_ext(private$..path))

      io <- switch(type,
                   txt = IOText$new(),
                   csv = IOCSV$new(),
                   rdata = IORdata$new(),
                   rds = IORDS$new())

      return(io)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$IOFactory(self)
    }
  )
)
