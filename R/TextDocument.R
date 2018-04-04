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

    getStats = function() {
      sentences <- sum(quanteda::nsentence(private$..content))
      words <- sum(quanteda::ntokens(private$..content))
      types <- sum(quanteda::ntype(private$..content))
      characters <- sum(nchar(private$..content))
      avgSentLen <- words / sentences
      avgWordLen <- characters / words
      datetime <- Sys.time()
      k <- c("sentences", "words", "types", "characters",
             "average sentence length", "average word length",
             "datetime")
      v <- c(sentences, words, types, characters,
             avgSentLen, avgWordLen,  datetime)
      private$meta$setStats(key = k, value = v)
      return(TRUE)
    },

    processContent = function(x, note = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$objects <- x
      private$..params$classes$valid <- 'character'
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(cls = class(x)[1], method = 'content',
                         event = v$msg, level = "Error")
        stop()
      }

      # Update content, compute statistics and update state information
      private$..content <- private$compress(x)
      private$getStats()
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
      private$meta <- Meta$new(name, purpose)

      # Validate content
      private$..params <- list()
      private$..params$classes$objects <- x
      private$..params$classes$valid <- 'character'
      if (v$code == FALSE) {
        private$logR$log(cls = class(self)[1], method = 'initialize',
                         event = v$msg, level = 'Error')
        stop()
      }

      private$processContent(x, note = "Initialized content.")
      private$logR$log(cls = class(self)[1], method = 'initialize',
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
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textDocument(self)
    }
  )
)
