#------------------------------------------------------------------------------#
#                        Replace Abbreviations App                             #
#------------------------------------------------------------------------------#
#' ReplaceAbbreviationsApp
#'
#' \code{ReplaceAbbreviationsApp} Replaces abbreviations with long form.
#'
#' This is a wrapper for the replace_abbreviations function in the QDAP package.
#' Source \url{https://cran.r-project.org/web/packages/qdap/qdap.pdf}
#'
#' @usage ReplaceAbbreviationsApp$new(x, replace, ignoreCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Should case be ignored? Only applies to default dictionary.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceAbbreviationsApp <- R6::R6Class(
  classname = "ReplaceAbbreviationsApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..abbreviations = character(),
    ..ignoreCase = logical(),

    processDocument = function(document) {

      content <- document$content

      if (is.null(private$..abbreviations)) {
        document$content <- qdap::replace_abbreviation(text.var = content,
                                              ignore.case = private$..ignoreCase)

      } else {
        if ("data.frame" %in% class(private$..abbreviations)) {
          pattern <- as.character(private$..abbreviations[,1])
          if (ncol(private$..abbreviations) == 2) {
            replacement <- as.character(private$..abbreviations[,2])
          } else if (ncol(private$..abbreviations) == 1) {
            replacement <- as.character(private$..replacement)
          }
        } else {
          pattern <- paste("\\b", private$..abbreviations, "(?!\\w)", sep = "")
          replacement <- private$..replacement
        }

        document$content <- textclean::mgsub(x = content,  pattern = pattern,
                                    replacement = replacement,  fixed = FALSE,
                                    perl = TRUE)
      }
      private$logEvent(document)

      return(document)
    }
  ),

  public = list(
    initialize = function(x, abbreviations = NULL,
                          replacement = NULL, ignoreCase = TRUE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$kv$key <- abbreviations
      private$..params$kv$value <- replacement
      private$..params$kv$equalLen <- TRUE
      private$..params$logicals$variables <- c('ignoreCase')
      private$..params$logicals$values <- c(ignoreCase)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..abbreviations <- abbreviations
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase

      invisible(self)
    }
  )
)
