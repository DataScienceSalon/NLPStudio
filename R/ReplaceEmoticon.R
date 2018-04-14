#------------------------------------------------------------------------------#
#                              ReplaceEmoticon                                 #
#------------------------------------------------------------------------------#
#' ReplaceEmoticon
#'
#' \code{ReplaceEmoticon} Command for the ReplaceEmoticon class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoticon
#' class
#'
#' @usage ReplaceEmoticon$new(emoticons = NULL)
#'
#' @template textStudioParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoticon <- R6::R6Class(
  classname = "ReplaceEmoticon",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emoticons = data.table()
  ),

  public = list(
    initialize = function(emoticons = NULL) {
      private$loadDependencies()
      private$..emoticons <- emoticons
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceEmoticonApp$new(x, private$..emoticons)$execute()
      return(x)
    }
  )
)
