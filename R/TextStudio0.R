#==============================================================================#
#                               TextStudio0                                    #
#==============================================================================#
#' TextStudio0
#'
#' \code{TextStudio0} Abstract class  for the TextStudio family of classes.
#'
#' This abstract class defines a common interface and methods for the TextStudio
#' family of classes.
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio classes
#' @export
TextStudio0 <- R6::R6Class(
  classname = "TextStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..pattern = character(),
    ..replacement = character(),
    ..ignoreCase = logical(),
    ..perl = logical(),
    ..fixed = logical(),
    ..params = list(),

    logEvent = function(x) {
      event <- paste0(class(self)[1], " complete.")
      return(x$message(event = event))
    },

    processDocument = function(document) {
      document$text <- gsub(pattern = private$..pattern,
                      replacement = private$..replacement,
                      x = document$text,
                      ignore.case = private$..ignoreCase,
                      perl = private$..perl,
                      fixed = private$..fixed)
      private$logEvent(document)
      return(document)
    },

    processCorpus = function() {
      docs <- private$..x$getDocuments(key = 'classname', value = "Document")
      for (i in 1:length(docs)) {
        doc <- private$processDocument(docs[[i]])
        private$..x$addDocument(doc)
      }
      private$logEvent(private$..x)
      return(private$..x)
    }
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      if ("Corpus" %in% class(private$..x)) {
        private$..x <- private$processCorpus()

      } else {
        private$..x <- private$processDocument(private$..x)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$..x$message(event)
      private$logR$log(method = 'execute', event = event)

      return(private$..x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio0(self)
    }
  )
)
