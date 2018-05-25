#------------------------------------------------------------------------------#
#                              ReplaceWhite                           #
#------------------------------------------------------------------------------#
#' ReplaceWhite
#'
#' \code{ReplaceWhite} Command for the ReplaceWhite class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceWhite
#' class
#'
#' @usage ReplaceWhite$new(impartMeaning = TRUE)
#'
#' @template textStudioParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceWhite <- R6::R6Class(
  classname = "ReplaceWhite",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceWhiteApp$new(x)$execute()
      return(x)
    }
  )
)
