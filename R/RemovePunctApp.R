#------------------------------------------------------------------------------#
#                              Remove Punct App                                #
#------------------------------------------------------------------------------#
#' RemovePunctApp
#'
#' \code{RemovePunctApp} Removes Puncts.
#'
#' Removes Puncts  from text.
#'
#' @usage RemovePunctApp$new(x)$execute()
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
RemovePunctApp <- R6::R6Class(
  classname = "RemovePunctApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..keepApostrophe = logical(),
    ..keepHyphen = logical()
  ),

  public = list(
    initialize = function(x, keepApostrophe = TRUE, keepHyphen = TRUE, perl = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$logicals$variables <- list('keepApostrophe', 'keepHyphen')
      private$..params$logicals$values <- list(keepApostrophe, keepHyphen)
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..perl <- perl

      if (keepApostrophe & keepHyphen) {
        private$..pattern <- "(?![-'])[[:punct:]]"
      } else if (keepApostrophe) {
        private$..pattern <- "(?!['])[[:punct:]]"
      } else if (keepHyphen) {
        private$..pattern <- "(?![-])[[:punct:]]"
      }
      private$..replacement = " "
      private$..keepApostrophe <- keepApostrophe
      private$..keepHyphen <- keepHyphen

      invisible(self)
    }
  )
)
