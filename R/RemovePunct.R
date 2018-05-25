#------------------------------------------------------------------------------#
#                                RemovePunct                                   #
#------------------------------------------------------------------------------#
#' RemovePunct
#'
#' \code{RemovePunct} Command for the RemovePunct class.
#'
#' Class that encapsulates the command to execute an object of the RemovePunct
#' class
#'
#' @usage RemovePunct$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemovePunct <- R6::R6Class(
  classname = "RemovePunct",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..keepApostrophe = logical(),
    ..keepHyphen = logical()
  ),

  public = list(
    initialize = function(keepApostrophe = TRUE, keepHyphen = TRUE) {
      private$loadDependencies()
      private$..keepApostrophe = keepApostrophe
      private$..keepHyphen = keepHyphen
      invisible(self)
    },
    execute = function(x) {
      x <- RemovePunctApp$new(x, keepApostrophe = private$..keepApostrophe,
                              keepHyphen = private$..keepHyphen)$execute()
      return(x)
    }
  )
)
