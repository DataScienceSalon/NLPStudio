#' NGrammer
#'
#' \code{NGrammer} Class representing nGram Document objects.
#'
#' Class containing the nGramd representation of a Document object.
#'
#' @usage NGrammer <- NGrammer$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the NGrammer class.}
#'   \item{\code{content}}{Active binding used to set and retrieve NGrammer content. NGrammer
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   NGrammer content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return NGrammer object, containing the NGrammer for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family NGrammer Classes
#' @export
NGrammer <- R6::R6Class(
  classname = "NGrammer",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    nGramDocument = function(x, n, nGramType) {

      # Create NGRam object and initialize with document metadata
      nGrams <- NGram$new(x)
      nGrams <- Copy$new()$this(x, to = nGrams)
      name <- paste0(x$getName(), " ", nGramType)
      nGrams$setName(name)

      # Tokenize text and extract nGrams
      tokens <- NLPStudio::tokenize(x = x$text, tokenType = 'word')
      nGrams$content <- quanteda::tokens_ngrams(x = tokens, n = n, concatenator = " ")

      # Update metadata
      nGrams$setMeta(key = 'nGramType', value = nGramType, type = 'f')
      nGrams$setMeta(key = paste0(nGramType, "Count"), value = length(nGrams$content), type = 'q')
      nGrams$setMeta(key = paste0(nGramType, "Types"), value = length(unique(nGrams$content)), type = 'q')

      return(nGrams)
    },

    nGramCorpus = function(x, n, nGramType) {

      # Create NGramSet object and initialize with corpus metadata
      nGramSet <- NGramSet$new(x)
      nGramSet <- Copy$new()$this(x, to = nGramSet)
      name <- paste0(x$getName(), " ", nGramType)
      nGramSet$setName(name)

      # Extract ngrams for each Document object
      nGrams <- character()
      docs <- x$getDocuments()
      for (i in 1:length(docs)) {
        nGramObject <- private$nGramDocument(docs[[i]], n, nGramType)
        nGramSet$addNGram(nGramObject)
        nGrams <- c(nGrams, nGramObject$content)
      }

      # Update metadata
      nGramSet$setMeta(key = 'nGramType', value = nGramType, type = 'f')
      nGramSet$setMeta(key = paste0(nGramType, "Count"), value = length(nGrams), type = 'q')
      nGramSet$setMeta(key = paste0(nGramType, "Types"), value = length(unique(nGrams)), type = 'q')

      return(nGramSet)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            NGrammer                                     #
    #-------------------------------------------------------------------------#
    this = function(x, n) {

      # Validate Source Object
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      nGramType <- ifelse(n == 1, "Unigram",
                          ifelse(n == 2, "Bigram",
                                 ifelse(n == 3, "Trigram",
                                        ifelse(n == 4, "Quadgram",
                                               ifelse(n == 5, "QuintGram",
                                                      paste0(n, "Gram"))))))

      if (class(x)[1] == 'Corpus') {
        nGrams <- private$nGramCorpus(x, n = n, nGramType = nGramType)
      } else {
        nGrams <- private$nGramDocument(x, n = n, nGramType = nGramType)
      }

      event <- paste0("Created ", nGramType, " object from ", x$getName(), ".")
      private$logR$log(method = 'this', event = event)
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$nGrammer(self)
    }
  )
)
