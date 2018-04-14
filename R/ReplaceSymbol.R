#------------------------------------------------------------------------------#
#                                  ReplaceSymbol                               #
#------------------------------------------------------------------------------#
#' ReplaceSymbol
#'
#' \code{ReplaceSymbol} Command for the ReplaceSymbol class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceSymbol
#' class
#'
#' @usage ReplaceSymbol$new(dollar = FALSE, percent = TRUE)
#'
#' @template textStudioParams
#' @param dollar logical. If TRUE replaces dollar sign (\$) with "dollar".
#' @param percent logical. If TRUE replaces percent sign (\%) with "percent".
#' @param pound logical. If TRUE replaces pound sign (\#) with "number".
#' @param at logical. If TRUE replaces at sign (\@) with "at".
#' @param and logical. If TRUE replaces and sign (\&) with "and".
#' @param  with logical. If TRUE replaces with sign (w/) with "with"
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceSymbol <- R6::R6Class(
  classname = "ReplaceSymbol",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..dollar = logical(),
    ..percent = logical(),
    ..pound = logical(),
    ..at = logical(),
    ..and = logical(),
    ..with = logical()
  ),

  public = list(
    initialize = function(dollar = TRUE, percent = TRUE, pound = TRUE,
                          at = TRUE, and = TRUE, with = TRUE) {
      private$loadDependencies()
      private$..dollar <- dollar
      private$..percent <- percent
      private$..pound <- pound
      private$..at <- at
      private$..and <- and
      private$..with <- with
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceSymbolApp$new(x, dollar = private$..dollar,
                             percent = private$..percent,
                             pound = private$..pound,
                             at = private$..at,
                             and = private$..and,
                             with = private$..with)$execute()
      return(x)
    }
  )
)
