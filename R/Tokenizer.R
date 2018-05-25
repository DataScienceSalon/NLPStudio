#' Tokenizer
#'
#' \code{Tokenizer} Class representing tokenized Document objects.
#'
#' Class containing the tokenized representation of a Document object.
#'
#' @usage Tokenizer <- Tokenizer$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the Tokenizer class.}
#'   \item{\code{content}}{Active binding used to set and retrieve Tokenizer content. Tokenizer
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   Tokenizer content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return Tokenizer object, containing the Tokenizer for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Tokenizer Classes
#' @export
Tokenizer <- R6::R6Class(
  classname = "Tokenizer",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    tokenizeDocument = function(x, tokenType) {

      document <- Tokens$new(tokenType)
      document <- Copy$new()$this(x, to = document)
      name <- paste0(x$getName(), " (", tokenType, " tokens)")
      document$setName(name)

      document$content <- unlist(NLPStudio::tokenize(x = x$content, tokenType = tokenType))

      return(document)
    },

    tokenizeCorpus = function(x, tokenType) {

      corpus <- Clone$new()$this(x, reference = FALSE)
      name <- paste0(x$getName(), " (", tokenType, " tokens)")
      corpus$setName(name)

      nTokens <- 0
      docs <- x$getDocuments()
      for (i in 1:length(docs)) {
        document <- private$tokenizeDocument(docs[[i]], tokenType)
        nTokens <- nTokens + document$nTokens()
        corpus$addDocument(document)
      }

      corpus$setMeta(key = 'tokenType', value = tokenType, type = 'f')
      corpus$setMeta(key = paste0(tokenType, 's'), value = nTokens, type = 'q')
      corpus$setMeta(key = 'documents', value = length(docs), type = 'q')

      return(corpus)
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
    #                            Tokenizer                                    #
    #-------------------------------------------------------------------------#
    this = function(x, type = 'word') {

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

      if (grepl("^w", type, ignore.case = TRUE)) {
        type <- 'word'
      } else if (grepl("^s", type, ignore.case = TRUE)) {
        type <- 'sentence'
      } else if (grepl("^c", type, ignore.case = TRUE)) {
        type <- 'character'
      } else {
        event <- paste0("Invalid token type. Valid types are c('word', 'sentence',",
                        ", 'character'). See?", class(self)[1], " for further ",
                        "assistance.")
        private$logR$log(method = 'this', event = event, level = "Error")
        stop()
      }

      if (class(x)[1] == 'Corpus') {
        tokens <- private$tokenizeCorpus(x, tokenType = type)
      } else {
        tokens <- private$tokenizeDocument(x, tokenType = type)
      }

      event <- paste0("Created ", type, " tokens from ", x$getName(), ".")
      private$logR$log(method = 'this', event = event)
      return(tokens)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenizer(self)
    }
  )
)
