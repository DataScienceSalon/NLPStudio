#------------------------------------------------------------------------------#
#                         Replace Word Elongation App                          #
#------------------------------------------------------------------------------#
#' ReplaceWordElongationApp
#'
#' \code{ReplaceWordElongationApp}  Replace Word Elongations.
#'
#' A wrapper for \code{\link[textclean]{replace_word_elongation}} In informal writing
#' people may use a form of text embellishment to emphasize or alter word meanings
#' called elongation (a.k.a. "word lengthening"). For example, the use of "Whyyyyy" conveys
#' frustration. Other times the usage may be to be more sexy (e.g., "Heyyyy there"). Other times it
#' may be used for emphasis (e.g., "This is so gooood"). This function uses an augmented form of
#' Armstrong & Fogarty’s (2007) algorithm. The algorithm first attempts to replace the elongation
#' with known semantic replacements (optional; default is FALSE). After this the algorithm locates all
#' places were the same letter (case insensitive) appears 3 times consecutively. These elements are then
#' further processed. The matches are replaces via fgsub by first taking the elongation to it’s canonical
#' form (drop all > 1 consecutive letters to a single letter) and then replacing with the most common
#' word used in 2008 in Google’s ngram data set that takes the canonical form. If the canonical form
#' is not found in the Google data set then the canonical form is used as the replacement.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceWordElongationApp$new(x, impartMeaning = FALSE)$execute()
#'
#' @template textStudioParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceWordElongationApp} Returns a vector with word elongations replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceWordElongationApp <- R6::R6Class(
  classname = "ReplaceWordElongationApp",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    ..impartMeaning = logical(),

    processDocument = function(document) {
      document$content <- textclean::replace_word_elongation(x = document$content,
                                           impart.meaning = private$..impartMeaning)
      private$logEvent(document)
      return(document)
    }
  ),

  public = list(
    initialize = function(x, impartMeaning = FALSE) {

      private$loadDependencies()

      # Validate parameters
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('impartMeaning')
      private$..params$logicals$values <- c(impartMeaning)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..impartMeaning <- impartMeaning

      invisible(self)
    }
  )
)
