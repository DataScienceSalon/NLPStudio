#' Document
#'
#' \code{Document} Entity class for text documents.
#'
#' Entity class for text documents with methods for adding text from character
#' vectors.
#'
#' @usage skiReport <- Document$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL, purpose = NULL)}}{Initializes an object of the Document class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{overview()}}{Provides a subset of the metadata in a one-row data.frame format.
#'   This is used by the parent class's summary method.  }
#'  }
#'
#' @param name Character string containing the name for the Document object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the Documents
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return Document object, containing the Document text, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' report <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' avalanche <- Document$new(name = 'avalanche', purpose = 'raw')
#' avalance$text <- report
#' key <- c('genre', 'author', 'year')
#' value <- c('weather', 'chris jones', 2018)
#' avalanche$meta$setDescriptive(key = key value = value)
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  private = list(
    ..text = character(),

    compress = function(x) {
      memCompress(x, "g")
    },

    decompress = function(x) {
      strsplit(memDecompress(x, "g", asChar = TRUE), "\n")[[1]]
    },

    setQuant = function(x) {
      sentences <- sum(quanteda::nsentence(x))
      words <- sum(quanteda::ntoken(x))
      types <- sum(quanteda::ntype(x))
      characters <- sum(nchar(x))
      k <- c("sentences", "words", "types", "characters")
      v <- c(sentences, words, types, characters)
      private$meta$set(key = k, value = v, type = 'quant')
      return(TRUE)
    },

    processContent = function(x, note = NULL) {

      # Validate text
      if (!is.null(x)) {
        private$..params <- list()
        private$..params$classes$name <- list('x')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid <- list('character')
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'processContent',
                           event = v$msg, level = "Error")
          stop()
        }

        # Update text, compute statistics and update admin information
        private$..text <- private$compress(x)
        private$setQuant(x)
        private$meta$modified(event = note)
      }
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x = NULL, name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)

      # Validate text
      if (!is.null(x)) {
        private$..params <- list()
        private$..params$classes$name <- list('x')
        private$..params$classes$objects <- list(x)
        private$..params$classes$valid <- list('character')
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'initialize',
                           event = v$msg, level = "Error")
          stop()
        }
        private$processContent(x, note = "Initialized text.")
      }

      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Content Method                                #
    #-------------------------------------------------------------------------#
    text = function(x = NULL, note = NULL) {

      if (missing(x)) {
        return(private$decompress(private$..text))

      } else {
        private$processContent(x, note = note)
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
