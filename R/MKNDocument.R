#==============================================================================#
#                               MKNDocument                                    #
#==============================================================================#
#' MKNDocument
#'
#' \code{MKNDocument} Prepares text data for downstream processing.
#'
#' Gathers text data, creates sentence tokens and, if open is TRUE, converts
#' hapax legomenon to unknown tokens.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNDocument <- R6::R6Class(
  classname = "MKNDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = MKNStudio0,

  private = list(
    ..corpus = character(),

    convert = function() {

      # Extract and collapse private$..corpus text
      documents <- private$..corpus$getDocuments()
      text <- paste(unlist(lapply(documents, function(d) {
        d$text})), collapse = " ")

      # Create new Document object and copy metadata from the private$..corpus object
      private$..document <- Document$new(x = text)
      private$..document <- Copy$new()$this(x = private$..corpus, to = private$..document)

      # Set metadata
      name <- private$..lm$getName()
      name <- paste0(name, " Document")
      private$..document$setName(name)
      private$..document$setMeta(key = 'smoothing', value = "Modified Kneser-Ney",
                        type = 'f')
      private$..document$setMeta(key = 'modelType',
                       value = private$..size,
                       type = 'f')
      private$..document$setMeta(key = 'openVocabulary',
                       value = private$..lm$is.openVocabulary(),
                       type = 'f')
      private$..document$setMeta(key = 'lm',
                       value = 'Modified Kneser-Ney',
                       type = 'f')

      return(TRUE)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('MKN', 'Katz', 'SBO'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock language model (extract members read/modified in class)
      private$..lm <- x
      private$..open <- x$is.openVocabulary()
      private$..corpus <- x$getCorpus()
      private$..size <- x$getSize()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknDocument(self)
    }
  )
)
