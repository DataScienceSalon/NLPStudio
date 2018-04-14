#------------------------------------------------------------------------------#
#                              ReplaceWordElongation                           #
#------------------------------------------------------------------------------#
#' ReplaceWordElongation
#'
#' \code{ReplaceWordElongation} Command for the ReplaceWordElongation class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceWordElongation
#' class
#'
#' @usage ReplaceWordElongation$new(impartMeaning = TRUE)
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
ReplaceWordElongation <- R6::R6Class(
  classname = "ReplaceWordElongation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..impartMeaning = logical()
  ),

  public = list(
    initialize = function(impartMeaning = FALSE) {
      private$loadDependencies()

      # Validate parameters
      private$..params$logicals$variables <- c('impartMeaning')
      private$..params$logicals$values <- c(impartMeaning)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }
      private$..impartMeaning <- impartMeaning
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceWordElongationApp$new(x, impartMeaning = private$..impartMeaning)$execute()
      return(x)
    }
  )
)
