#' Token
#'
#' \code{Token} Tokens class
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
#' @param x Corpus object or a character vector containing text
#' @param n Numeric for the nGram method. Indicates the nGram size.
#' @return Tokenizer object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Tokenizer Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Token <- R6::R6Class(
  classname = "Token",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..tokenizedCorpus = character(),

    validate = function(x) {
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus', 'character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        stop()
      }

      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices("Token")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Character Tokens Method                           #
    #-------------------------------------------------------------------------#
    chars = function(x, tokenizer = 'tokenizer') {

      private$validate(x)

      if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          Tokenizer$new()$chars(x)$getCorpus()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerQ$new()$chars(x)$getCorpus()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'chars', event = event, level = "Error")
        stop()
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Word Tokens Method                              #
    #-------------------------------------------------------------------------#
    words = function(x, tokenizer = 'tokenizer') {

      private$validate(x)

      if (grepl("^o", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerNLP$new()$words(x)$getCorpus()

      } else if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          Tokenizer$new()$words(x)$getCorpus()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerQ$new()$words(x)$getCorpus()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('openNLP', 'tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'words', event = event, level = "Error")
        stop()
      }
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                       Sentence Tokens Method                            #
    #-------------------------------------------------------------------------#
    sentences = function(x, tokenizer = 'openNLP') {

      private$validate(x)

      if (grepl("^o", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerNLP$new()$sentences(x)$getCorpus()

      } else if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          Tokenizer$new()$sentences(x)$getCorpus()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerQ$new()$sentences(x)$getCorpus()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('openNLP', 'tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'sentences', event = event, level = "Error")
        stop()
      }
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                       Paragraph Tokens Method                           #
    #-------------------------------------------------------------------------#
    paragraphs = function(x, tokenizer = 'tokenizer', paragraphBreak = "\n\n") {

      private$validate(x)

      if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          Tokenizer$new()$paragraphs(x, paragraphBreak = paragraphBreak)$getCorpus()

      } else {
        event <- paste0("Tokenizer, ", tokenizer, ", does not support paragraph ",
                        "token functionality. See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'paragraphs', event = event, level = "Error")
        stop()
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         nGram Tokens Method                             #
    #-------------------------------------------------------------------------#
    nGrams = function(x, tokenizer = 'tokenizer', n = 3, nGramDelim = " ") {

      private$validate(x)

      # Validate N
      private$..params <- list()
      private$..params$range$variable <- c('n')
      private$..params$range$value <- c(n)
      private$..params$range$low <- 1
      private$..params$range$high <- 10
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'nGrams',
                         event = v$msg, level = "Error")
        stop()
      }

      if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          Tokenizer$new()$nGrams(x, n = n, nGramDelim = nGramDelim)$getCorpus()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokenizedCorpus <-
          TokenizerQ$new()$nGrams(x, n = n, nGramDelim = nGramDelim)$getCorpus()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'nGrams', event = event, level = "Error")
        stop()
      }
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
      tokens <- unlist(lapply(documents, function(d) {
        unlist(d$content)}))
      return(tokens)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$token(self)
    }
  )
)
