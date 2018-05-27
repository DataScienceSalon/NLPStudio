#' Corpus
#'
#' \code{Corpus} Entity class for text documents.
#'
#' Entity class for text documents with methods for adding text from character
#' vectors.
#'
#' @usage skiReport <- Corpus$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the Corpus class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the Corpus object.}
#'  }
#'
#' @param name Character string containing the name for the Corpus object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the Corpuss
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return Corpus object, containing the Corpus text, the metadata and
#' the methods to manage both.
#'
#'
#' @examples
#' report <- c("SAN FRANCISCO  — She was snowboarding with her boyfriend when ",
#'           "she heard someone scream 'Avalanche!'",
#'           "Then John, 39, saw 'a cloud of snow coming down.'")
#' avalanche <- Corpus$new(name = 'avalanche', purpose = 'raw')
#' avalance$content <- report
#' key <- c('genre', 'author', 'year')
#' value <- c('weather', 'chris jones', 2018)
#' avalanche$meta$setDescriptive(key = key value = value)
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  private = list(

    setQuant = function() {

      quant <- rbindlist(lapply(private$..documents, function(d) {
        meta <- d$getMeta()
        meta$quant
      }))

      documents <- nrow(quant)
      vectors <- sum(quant$vectors)
      sentences <- sum(quant$sentences)
      types <- sum(quant$types)
      words <- sum(quant$words)
      characters <- sum(quant$characters)

      if (documents > 0 ) {
        private$meta$set(key = 'documents', value = documents, type = 'q')
      }

      if (vectors > 0 ) {
        private$meta$set(key = 'vectors', value = vectors, type = 'q')
      }

      if (sentences > 0 ) {
        private$meta$set(key = 'sentences', value = sentences, type = 'q')
      }

      if (types > 0 ) {
        private$meta$set(key = 'types', value = types, type = 'q')
      }

      if (words > 0 ) {
        private$meta$set(key = 'words', value = words, type = 'q')
      }

      if (characters > 0 ) {
        private$meta$set(key = 'characters', value = characters, type = 'q')
      }

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeQuantMeta = function(verbose = TRUE) {

      private$setQuant()
      quant <- private$meta$get(type = 'quant')
      if (verbose) {
        if (!is.null(quant)) {
          NLPStudio::printHeading(text = "Quantitative Metadata", symbol = "-")
          quantDf <- as.data.frame(quant, stringsAsFactors = FALSE, row.names = NULL)
          print(quantDf, row.names = FALSE)
        }
      }
      return(quant)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Document Management                           #
    #-------------------------------------------------------------------------#
    getDocuments = function() { return(private$..documents) },

    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Tokens'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    removeDocument = function(x) {
      private$detach(x)
      invisible(self)
    },

    purgeDocuments = function() {
      private$..documents <- list()
      private$..inventory <- data.frame()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
    }
  )
)
