#------------------------------------------------------------------------------#
#                             ReplaceInternetSlang                             #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlang
#'
#' \code{ReplaceInternetSlang} Command for the ReplaceInternetSlang class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceInternetSlang
#' class
#'
#' @usage ReplaceInternetSlang$new(slang = NULL, replacement = NULL, ignoreCase = TRUE)
#'
#' @template textStudioParams
#' @param slang A vector of slang strings to replace.
#' @param replacement A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case)
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceInternetSlang <- R6::R6Class(
  classname = "ReplaceInternetSlang",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(slang = NULL, replacement = NULL, ignoreCase = TRUE) {

      private$loadDependencies()
      private$..slang <- slang
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase

      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceInternetSlangApp$new(x, slang = private$..slang,
                                    replacement = private$..replacement,
                                    ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
