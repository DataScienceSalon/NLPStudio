#------------------------------------------------------------------------------#
#                            RemovePunct                                       #
#------------------------------------------------------------------------------#
#' RemovePunct
#'
#' \code{RemovePunct} Command for the RemovePunct class.
#'
#' Class that encapsulates the command to execute an object of the RemovePunct
#' class
#'
#' @usage RemovePunct$new(endmark = FALSE, apostrophe = FALSE)
#'
#' @template textStudioParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
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
    ..endmark = logical(),
    ..apostrophe = logical()
  ),

  public = list(
    initialize = function(endmark = FALSE, apostrophe = FALSE) {
      private$loadDependencies()
      private$..endmark <- endmark
      private$..apostrophe <- apostrophe
      invisible(self)
    },
    execute = function(x) {
      x <- RemovePunctApp$new(x, endmark = private$..endmark,
                                 apostrophe = private$..apostrophe)$execute()
      return(x)
    }
  )
)
