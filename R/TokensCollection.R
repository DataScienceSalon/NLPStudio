#' TokensCollection
#'
#' \code{TokensCollection} Class containing a collection of Tokens objects
#'
#' Class contains Tokens objects
#'
#' @usage skiReport <- TokensCollection$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the TokensCollection class.}
#'   \item{\code{word()}}{Creates word tokens.}
#'   \item{\code{sentence()}}{Creates sentence tokens.}
#'   \item{\code{char()}}{Creates character tokens.}
#'   \item{\code{getTokens()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the TokensCollection object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the TokensCollections
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return TokensCollection object, containing the TokensCollection text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Tokens Classes
#' @export
TokensCollection <- R6::R6Class(
  classname = "TokensCollection",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  private = list(

    #-------------------------------------------------------------------------#
    #                           Tokenize Methods                              #
    #-------------------------------------------------------------------------#
    tokenize = function(tokenType = 'word') {
      docs <- private$..x$getDocument()
      ntokens <- 0
      for (i in 1:length(docs)) {
        tokensObject <- switch(tokenType,
                         word = Tokens$new(docs[[i]])$word(),
                         sentence = Tokens$new(docs[[i]])$sentence(),
                         character = Tokens$new(docs[[i]])$char())
        self$addDocument(tokensObject)
        tokens <- tokensObject$getTokens()

        if (class(tokens)[1] == 'tokens') {
          ntokens <- ntokens + ntoken(tokens)
        } else {
          ntokens <- ntokens + length(tokens)
        }
      }
      private$meta$set(key = tokenType, value = ntokens, type = 'q')
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
    initialize = function(x) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    getTokens = function() {
      tokens <- lapply(private$..documents, function(d) {
        d$getTokens()
      })
      return(tokens)
    },

    #-------------------------------------------------------------------------#
    #                           Word Tokens                                   #
    #-------------------------------------------------------------------------#
    word = function() {

      private$tokenize(tokenType = 'word')
      event <- paste0("Created word token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Word Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'word', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Sentence Tokens                               #
    #-------------------------------------------------------------------------#
    sentence = function() {

      private$..type <- "sentence"
      private$tokenize(tokenType = 'sentence')
      event <- paste0("Created sentence token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Sentence Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'sentence', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Character Tokens                                  #
    #-------------------------------------------------------------------------#
    char = function(x) {

      private$tokenize(tokenType = 'character')
      event <- paste0("Created character token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Character Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'character', event = event)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Management                           #
    #-------------------------------------------------------------------------#
    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Tokens'))
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
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokensCollection(self)
    }
  )
)
