#------------------------------------------------------------------------------#
#                         Replace Contractions App                             #
#------------------------------------------------------------------------------#
#' ReplaceContractionsApp
#'
#' \code{ReplaceContractionsApp}  Replace contractions.
#'
#' A wrapper for \code{\link[textclean]{replace_contraction}} that
#' replaces contractions with long form.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceContractionsApp$new(x, contractions = lexicon::key_contractions, ignoreCase = TRUE)$execute()
#'
#' @template textStudioParams
#' @param contractions Character string of contractions to be matched in the
#' given character vector. If NULL, the default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param replacement Character string equal in length to contractions containing
#'  the long forms of the contractions.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, contractions is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param orderPattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{contractions} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{contractions = c("the", "then")} resorts to
#' search for "then" first).
#' @param \dots ignored.
#'
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceContractionsApp} Returns a vector with contractions replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceContractionsApp <- R6::R6Class(
  classname = "ReplaceContractionsApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..contractions = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character(),

    processDocument = function(document) {

      content <- document$content

      if (is.null(private$..contractions)) {
        document$content <- textclean::replace_contraction(x = content,
                                                ignore.case = private$..ignoreCase)
      } else {
        document$content <- textclean::mgsub(x = content,
                                    pattern = private$..contractions,
                                    replacement = private$..replacement,
                                    leadspace = private$..leadspace,
                                    trailspace = private$..trailspace,
                                    fixed = private$..fixed,
                                    trim = private$..trim,
                                    order.pattern = private$..orderPattern)
      }
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, contractions = NULL, replacement = NULL, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          orderPattern = fixed) {

      private$loadDependencies()

      # Validate parameters
      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$kv$key <- contractions
      private$..params$kv$value <- replacement
      private$..params$kv$equalLen <- TRUE
      private$..params$logicals$variables <- c('leadSpace', 'trailspace', 'fixed', 'trim', 'orderPattern')
      private$..params$logicals$values <- c(leadspace, trailspace, fixed, trim, orderPattern)
      if (private$validate(x)$code == FALSE) stop()

      private$..x <- x
      private$..contractions <- contractions
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern

      invisible(self)
    }
  )
)
