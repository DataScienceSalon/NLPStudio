#' Corpus
#'
#' \code{Corpus} Entity class for text documents.
#'
#' Entity class for text documents with methods for adding content from character
#' vectors.
#'
#' @usage skiReport <- Corpus$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL, purpose = NULL)}}{Initializes an object of the Corpus class.}
#'   \item{\code{content(x, note = NULL)}}{Method for obtaining/adding/updating content. If no
#'   parameters are presented, the current content is returned.  Otherwise, the content
#'   is updated with the contents of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the Corpus object.}
#'  }
#'
#' @param name Character string containing the name for the Corpus object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' content method. The contents of the note variable are written to the Corpuss
#' log. This is used to track changes to the content, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return Corpus object, containing the Corpus content, the metadata and
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
  inherit = Collection0,

  private = list(

    getQuant = function() {

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
      private$meta$setQuant(key = k, value = v)
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeQuant = function(verbose = TRUE) {
      private$getQuant()
      quant <- private$meta$getQuant()
      if (verbose) {
        if (!is.null(quant)) {
          quantDf <- as.data.frame(quant, stringsAsFactors = FALSE, row.names = NULL)
          cat("\n\nQuantitative:\n")
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
    initialize = function(x, name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             addDocument                                 #
    #-------------------------------------------------------------------------#
    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('TextDocument')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }

      # Get document credentials, add document and update inventory
      credentials <- x$getIdentity()
      private$..documents[[credentials$id]] <- x
      credentials <- as.data.frame(credentials, stringsAsFactors = FALSE, row.names = NULL)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      event <- paste0("Attached ", x$getName(), " object to ", self$getName(), ".")
      private$meta$modified(event = event)
      private$logR$log(method = 'addDocument',
                       event = event)

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
