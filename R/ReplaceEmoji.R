#------------------------------------------------------------------------------#
#                              ReplaceEmoji                                    #
#------------------------------------------------------------------------------#
#' ReplaceEmoji
#'
#' \code{ReplaceEmoji} Command for the ReplaceEmoji class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoji
#' class
#'
#' @usage ReplaceEmoji$new(emojis = NULL)
#'
#' @template textStudioParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoji <- R6::R6Class(
  classname = "ReplaceEmoji",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..emojis = data.table()
  ),

  public = list(
    initialize = function(emojis = NULL) {
      private$loadDependencies()
      private$..emojis <- emojis
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceEmojiApp$new(x, private$..emojis)$execute()
      return(x)
    }
  )
)
