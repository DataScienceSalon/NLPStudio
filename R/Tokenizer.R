#' Tokenizer
#'
#' \code{Tokenizer} Wrapper class for the tokenizer package.
#'
#' Creates character, word, sentence and paragraph tokens and nGrams using the
#' tokenizer package.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}{Initializes an object of the Tokenizer class.}}
#'   \item{\code{character(x)}}
#'   \item{\code{word(x)}}
#'   \item{\code{sentence(x)}}
#'   \item{\code{paragraph(x)}}
#'   \item{\code{nGrams(x, n)}}
#'  }
#'
#' @param x Corpus object
#' @param n Numeric for the nGram method. Indicates the nGram size.
#' @return Tokenizer object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Tokenizer Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Tokenizer <- R6::R6Class(
  classname = "Tokenizer",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..tokens = character(),
    ..nGrams = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),

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
    tokenize = function(x, n, tokenUnit, stopwords = character(),
                        paragraphBreak = "\n\n",  nGramDelim = " ") {

      if (grepl("^c", tokenUnit, ignore.case = TRUE)) {
        return(unlist(tokenizers::tokenize_characters(x = x,
                                               lowercase = FALSE,
                                               strip_non_alphanum = FALSE,
                                               simplify = FALSE)))
      } else if (grepl("^w", tokenUnit, ignore.case = TRUE)) {
        return(unlist(tokenizers::tokenize_words(x = x,
                                   lowercase = FALSE,
                                   strip_punct = FALSE,
                                   strip_numeric = FALSE,
                                   simplify = FALSE)))
      } else if (grepl("^s", tokenUnit, ignore.case = TRUE)) {
        return(unlist(tokenizers::tokenize_sentences(x = x,
                                              lowercase = FALSE,
                                              strip_punct = FALSE,
                                              simplify = FALSE)))
      } else if (grepl("^p", tokenUnit, ignore.case = TRUE)) {
        return(unlist(tokenizers::tokenize_paragraphs(x = x,
                                              paragraph_break = paragraphBreak,
                                              simplify = FALSE)))
      } else if (grepl("^n", tokenUnit, ignore.case = TRUE)) {
        return(unlist(tokenizers::tokenize_ngrams(x = x, lowercase = FALSE,
                                           n = n, stopwords = stopwords,
                                           ngram_delim = nGramDelim,
                                           simplify = FALSE)))
      } else {
        event <- paste0("Invalid token unit. Must be c('character', 'word',",
                        " 'sentence', 'paragraph'). See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'tokenize', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                           Execute Method                                #
    #-------------------------------------------------------------------------#
    execute = function(tokenUnit, n = NULL, paragraphBreak = "\n\n",
                       stopwords = character(), nGramDelim = " ") {

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
      private$..tokens <- Clone$new()$this(private$..x, reference = FALSE,
                                           content = FALSE)

      # Initialize counts
      totalCounts <- 0

      documents <- private$..x$getDocuments()

      for (i in 1:length(documents)) {

        # Clone document
        document <- Clone$new()$this(documents[[i]],  content = TRUE)


        # Tokenize
        document$content <- private$tokenize(document$content,
                                             n = n,
                                             paragraphBreak = paragraphBreak,
                                             tokenUnit = tokenUnit,
                                             stopwords = stopwords,
                                             nGramDelim = nGramDelim)

        # Get Counts
        counts <- length(document$content)
        totalCounts <- totalCounts + counts

        # Update documents metadata
        name <- document$getName()
        name <- paste0(name, " (", tokenType, " Tokens)")
        document$setName(name)
        document$setMeta(key = 'tokenizer', value = 'Tokenizer package', type = 'f')
        document$setMeta(key = 'tokenUnit', value = tokenType, type = 'f')
        document$setMeta(key = paste(tokenType, 'Tokens'), value = counts, type = 'q')

        private$..tokens$addDocument(document)

      }

      # Update corpus metadata
      name <- private$..tokens$getName()
      name <- paste0(name, " (", tokenType," Tokens)")
      private$..tokens$setName(name)
      private$..tokens$setMeta(key = 'tokenizer', value = 'Tokenizer package', type = 'f')
      private$..tokens$setMeta(key = 'tokenUnit', value = tokenType, type = 'f')
      private$..tokens$setMeta(key = paste(tokenType, 'Tokens'), value = totalCounts, type = 'q')
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices("Tokenizer")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Character Tokens Method                          #
    #-------------------------------------------------------------------------#
    chars = function(x) {

      private$validate(x)

      private$..x <- x

      private$execute(x, tokenUnit = 'Character')

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
    paragraphs = function(x, paragraphBreak = "\n\n") {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'Paragraph', paragraphBreak = paragraphBreak)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            NGrams Method                                #
    #-------------------------------------------------------------------------#
    nGrams = function(x, n = 3, stopwords = character(), nGramDelim = " ") {

      private$validate(x)
      private$..x <- x
      private$execute(tokenUnit = 'nGram', n = n,  stopwords = stopwords,
                      nGramDelim = nGramDelim)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Get Method                                #
    #-------------------------------------------------------------------------#
    getTokens = function() private$..tokens,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenizer(self)
    }
  )
)
