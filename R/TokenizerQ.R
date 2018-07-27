#' TokenizerQ
#'
#' \code{TokenizerQ} Wrapper class for the token functionality in the Quanteda package.
#'
#' Creates character, word, sentence and paragraph tokens and nGrams using the
#' tokenizer package.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}{Initializes an object of the TokenizerQ class.}}
#'   \item{\code{character(x)}}
#'   \item{\code{word(x)}}
#'   \item{\code{sentence(x)}}
#'   \item{\code{paragraph(x)}}
#'   \item{\code{nGrams(x, n)}}
#'  }
#'
#' @param x Corpus object
#' @param n Numeric for the nGram method. Indicates the nGram size.
#' @return TokenizerQ object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus TokenizerQ Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
TokenizerQ <- R6::R6Class(
  classname = "TokenizerQ",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..tokenizedCorpus = character(),
    ..nGrams = character(),

    #-------------------------------------------------------------------------#
    #                           Validation Method                             #
    #-------------------------------------------------------------------------#
    validate = function(x) {
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        stop()
      }

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Tokenize Method                               #
    #-------------------------------------------------------------------------#
    tokenize = function(x, n, tokenUnit, nGramDelim = " ") {

      if (grepl("^c", tokenUnit, ignore.case = TRUE)) {
        return(as.character(quanteda::tokens(x = x, what = 'character',
                                remove_numbers = FALSE,
                                remove_punct = FALSE,
                                remove_symbols = FALSE,
                                remove_separators = TRUE,
                                remove_twitter = FALSE,
                                remove_hyphens = FALSE,
                                remove_url = FALSE)))
      } else if (grepl("^w", tokenUnit, ignore.case = TRUE)) {
        return(as.character(quanteda::tokens(x = x, what = 'word',
                                remove_numbers = FALSE,
                                remove_punct = FALSE,
                                remove_symbols = FALSE,
                                remove_separators = TRUE,
                                remove_twitter = FALSE,
                                remove_hyphens = FALSE,
                                remove_url = FALSE)))
      } else if (grepl("^s", tokenUnit, ignore.case = TRUE)) {
        return(as.character(quanteda::tokens(x = x, what = 'sentence',
                                remove_numbers = FALSE,
                                remove_punct = FALSE,
                                remove_symbols = FALSE,
                                remove_separators = TRUE,
                                remove_twitter = FALSE,
                                remove_hyphens = FALSE,
                                remove_url = FALSE)))
      } else if (grepl("^n", tokenUnit, ignore.case = TRUE)) {
        tokenObject <- quanteda::tokens(x,what = 'word')
        return(as.character(quanteda::tokens_ngrams(x = tokenObject, n = n,
                                       concatenator = nGramDelim)))
      } else {
        event <- paste0("Invalid token unit. Must be c('character', 'word',",
                        " 'sentence'). See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'tokenize', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                           Execute Method                                #
    #-------------------------------------------------------------------------#
    execute = function(tokenUnit, n = NULL, nGramDelim = " ") {

      # Format nGram Type
      tokenType <- tokenUnit
      if (grepl("^n", tokenUnit, ignore.case = TRUE)) {
        if (n == 1)  tokenType <- "Unigrams"
        if (n == 2)  tokenType <- "Bigrams"
        if (n == 3)  tokenType <- "Trigrams"
        if (n == 4)  tokenType <- "Quadgrams"
        if (n == 5)  tokenType <- "Quintgrams"
      }
      # Create tokens object
      private$..tokenizedCorpus <- Clone$new()$this(private$..x, reference = FALSE,
                                           content = FALSE)

      # Initialize counts
      totalCounts <- 0

      documents <- private$..x$getDocuments()

      for (i in 1:length(documents)) {

        # Clone document
        document <- Clone$new()$this(documents[[i]],  content = TRUE)

        # Tokenize
        document$content <- unlist(lapply(document$content, function(d) {
          private$tokenize(d, n = n, tokenUnit = tokenUnit,
                           nGramDelim = nGramDelim)
        }))

        # Get Counts
        counts <- length(document$content)
        totalCounts <- totalCounts + counts

        # Update documents metadata
        name <- document$getName()
        name <- paste0(name, " (", tokenType, " Tokens)")
        document$setName(name)
        document$setMeta(key = 'tokenizer', value = 'quanteda package', type = 'f')
        document$setMeta(key = 'tokenUnit', value = tokenType, type = 'f')
        document$setMeta(key = paste(tokenType, 'Tokens'), value = counts, type = 'q')

        private$..tokenizedCorpus$addDocument(document)

      }

      # Update corpus metadata
      name <- private$..tokenizedCorpus$getName()
      name <- paste0(name, " (", tokenType," Tokens)")
      private$..tokenizedCorpus$setName(name)
      private$..tokenizedCorpus$setMeta(key = 'tokenizer', value = 'quanteda package', type = 'f')
      private$..tokenizedCorpus$setMeta(key = 'tokenUnit', value = tokenType, type = 'f')
      private$..tokenizedCorpus$setMeta(key = paste(tokenType, 'Tokens'), value = totalCounts, type = 'q')
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices("TokenizerQ")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Character Tokens Method                          #
    #-------------------------------------------------------------------------#
    chars = function(x) {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'Character')

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Word Tokens Method                            #
    #-------------------------------------------------------------------------#
    words = function(x) {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'Word')

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Sentence Tokens Method                            #
    #-------------------------------------------------------------------------#
    sentences = function(x) {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'Sentence')

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Paragraph Tokens Method                           #
    #-------------------------------------------------------------------------#
    paragraphs = function(x) {

      stop("This method is not implemented for this class." )

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            NGrams Method                                #
    #-------------------------------------------------------------------------#
    nGrams = function(x, n = 3, nGramDelim = " ") {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'nGram', n = n, nGramDelim = nGramDelim)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Get Methods                               #
    #-------------------------------------------------------------------------#
    getCorpus = function() private$..tokenizedCorpus,
    getNGrams = function() {
      documents <- private$..tokenizedCorpus$getDocuments()
      nGrams <- unlist(lapply(documents, function(d) {
        unlist(d$content)}))
      return(nGrams)
    },
    getTokens = function() {
      documents <- private$..tokenizedCorpus$getDocuments()
      tokens <- unlist(lapply(documents, function(d) {d$content}))
      return(tokens)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenizerQ(self)
    }
  )
)
