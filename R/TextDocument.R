#' TextDocument
#'
#' \code{TextDocument} Entity class for text documents.
#'
#' Entity class for text documents with methods for adding content from character
#' vectors.
#'
#' @usage skiReport <- TextDocument$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL, purpose = NULL)}}{Initializes an object of the TextDocument class.}
#'   \item{\code{content(x, note = NULL)}}{Method for obtaining/adding/updating content. If no
#'   parameters are presented, the current content is returned.  Otherwise, the content
#'   is updated with the contents of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the TextDocument object.}
#'  }
#'
#' @param name Character string containing the name for the TextDocument object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' content method. The contents of the note variable are written to the TextDocuments
#' log. This is used to track changes to the content, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return TextDocument object, containing the TextDocument content, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' report <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' avalanche <- TextDocument$new(name = 'avalanche', purpose = 'raw')
#' avalance$content <- report
#' key <- c('genre', 'author', 'year')
#' value <- c('weather', 'chris jones', 2018)
#' avalanche$meta$setCustom(key = key value = value)
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
TextDocument <- R6::R6Class(
  classname = "TextDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  private = list(
    ..content = character(),

    compress = function(x) {
      memCompress(x, "g")
    },

    decompress = function(x) {
      strsplit(memDecompress(x, "g", asChar = TRUE), "\n")[[1]]
    },

    getStats = function(x) {
      sentences <- sum(quanteda::nsentence(x))
      words <- sum(quanteda::ntoken(x))
      types <- sum(quanteda::ntype(x))
      characters <- sum(nchar(x))
      avgSentLen <- words / sentences
      avgWordLen <- characters / words
      k <- c("sentences", "words", "types", "characters",
             "average sentence length", "average word length")
      v <- c(sentences, words, types, characters,
             avgSentLen, avgWordLen)
      private$meta$setStats(key = k, value = v)
      return(TRUE)
    },

    validateContent = function(x, method) {
      private$..params <- list()
      private$..params$classes$name <- list('content')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('character')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = method,
                          event = v$msg, level = "Error")
      }
      return(v$code)
    },

    processContent = function(x, note = NULL) {

      # Validate class of object.
      if (private$validateContent(x, method = 'processContent') == FALSE) stop()

      # Update content, compute statistics and update state information
      private$..content <- private$compress(x)
      private$getStats(x)
      private$meta$modified(event = note)
      return(TRUE)
    }

  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL, purpose = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name, purpose = purpose)

      # Validate content
      if (private$validateContent(x, method = 'initialize') == FALSE) stop()

      # Process content and log
      private$processContent(x, note = "Initialized content.")
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Content Method                                #
    #-------------------------------------------------------------------------#
    content = function(x, note = NULL) {

      if (missing(x)) {
        return(private$decompress(private$..content))

      } else {
        private$processContent(x, note = note)
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Metadata Method                               #
    #-------------------------------------------------------------------------#


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textDocument(self)
    }
  )
)
