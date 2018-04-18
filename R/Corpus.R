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
#' avalance$text <- report
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
  inherit = Collection0,

  private = list(

    setQuant = function() {

      quant <- rbindlist(lapply(private$..documents, function(d) {
        meta <- d$getMeta()
        meta$quant
      }))

      documents <- nrow(quant)
      sentences <- sum(quant$sentences)
      types <- sum(quant$types)
      words <- sum(quant$words)
      characters <- sum(quant$characters)

      k <- c("documents", "sentences", "words", "types", "characters")
      v <- c(documents, sentences, words, types, characters)
      private$meta$set(key = k, value = v, type = 'quant')
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeQuant = function(verbose = TRUE) {
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
    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'File', 'TermFreq',
                                             'Tokens', 'POS', 'TokensCollection'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                             Text Method                                 #
    #-------------------------------------------------------------------------#
    text = function(x = NULL, note = NULL) {
      docs <- self$getDocument()
      if (is.null(x)) {
        return(as.character(lapply(docs, function(d) { d$text() })))
      } else {
        if (length(docs) == length(x)) {
          if (length(note) == 1) note <- rep(note, length(docs))
          for (i in 1:length(docs)) {
            docs[[i]]$text(x = x[i], note = note[i])
          }
        } else {
          event <- paste0("The 'x' parameter must be a list of texts with ",
                          "length equal to the number of Text documents ",
                          "in the Corpus. See?", class(self)[1],
                          " for further assistance.")
          private$logR$log(method = 'text', event = event, level = "Error")
          stop()
        }
      }
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
    }
  )
)
