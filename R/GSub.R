#------------------------------------------------------------------------------#
#                                   GSub                                       #
#------------------------------------------------------------------------------#
#' GSub
#'
#' \code{GSub} Command for the GSub class.
#'
#' Class that encapsulates the command to execute an object of the GSub
#' class
#'
#' @usage GSub$new(pattern, replacement, ignoreCase = TRUE)
#'
#' @template textStudioParams
#' @param pattern Character string containing a regular expression
#' (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param replacement a replacement for matched pattern in gsub
#' @param ignoreCase Logical if FALSE, the pattern matching is case sensitive
#' and if TRUE, case is ignored during matching.
#' @param perl Logical, if true, Perl-compatible regexps will be used.
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
GSub <- R6::R6Class(
  classname = "GSub",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,


  public = list(
    initialize = function(pattern, replacement, ignoreCase = FALSE, perl = TRUE,
                          fixed = FALSE) {
      private$loadDependencies()

      # Validate parameters
      private$..params$logical$variables <- c("ignoreCase", "perl", "fixed")
      private$..params$logical$values <- c(ignoreCase, perl, fixed)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..pattern <- pattern
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..perl <- perl
      private$..fixed <- fixed
      invisible(self)
    },
    execute = function(x) {
      x <- GSubApp$new(x, pattern = private$..pattern,
                       replacement = private$..replacement,
                       ignoreCase = private$..ignoreCase,
                       perl = private$..perl,
                       fixed = private$..fixed)$execute()
      return(x)
    }
  )
)
