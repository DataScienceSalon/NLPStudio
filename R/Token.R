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
    ..tokens = character(),

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
    initialize = function(x) {
      private$loadServices()
      private$validate(x)
      private$..x <- x
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Character Tokens Method                           #
    #-------------------------------------------------------------------------#
    chars = function(tokenizer = 'tokenizer') {

      if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          Tokenizer$new(x = private$..x)$chars()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerQ$new(x = private$..x)$chars()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'chars', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                         Word Tokens Method                              #
    #-------------------------------------------------------------------------#
    words = function(tokenizer = 'tokenizer') {

      if (grepl("^o", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerNLP$new(x = private$..x)$words()

      } else if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          Tokenizer$new(x = private$..x)$words()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerQ$new(x = private$..x)$words()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('openNLP', 'tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'words', event = event, level = "Error")
        stop()
      }
    },
    #-------------------------------------------------------------------------#
    #                       Sentence Tokens Method                            #
    #-------------------------------------------------------------------------#
    sentences = function(tokenizer = 'openNLP') {

      if (grepl("^o", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerNLP$new(x = private$..x)$sentences()

      } else if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          Tokenizer$new(x = private$..x)$sentences()

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerQ$new(x = private$..x)$sentences()
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('openNLP', 'tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'sentences', event = event, level = "Error")
        stop()
      }
    },
    #-------------------------------------------------------------------------#
    #                       Paragraph Tokens Method                           #
    #-------------------------------------------------------------------------#
    paragraphs = function(tokenizer = 'tokenizer', paragraphBreak = "\n\n") {

      if (grepl("^t", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          Tokenizer$new(x = private$..x)$paragraphs(paragraphBreak = paragraphBreak)

      } else {
        event <- paste0("Tokenizer, ", tokenizer, ", does not support paragraph ",
                        "token functionality. See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'paragraphs', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                         nGram Tokens Method                             #
    #-------------------------------------------------------------------------#
    nGrams = function(tokenizer = 'tokenizer', n = 3, nGramDelim = " ") {

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
        private$..tokens <-
          Tokenizer$new(x = private$..x)$nGrams(n = n,
                                                nGramDelim = nGramDelim)

      } else if (grepl("^q", tokenizer, ignore.case = TRUE)) {
        private$..tokens <-
          TokenizerQ$new(x = private$..x)$nGrams(n = n,
                                                 nGramDelim = nGramDelim)
      } else {
        event <- paste("Invalid tokenizer type. Valid types include ",
                       "c('tokenizer', 'quanteda').  See ?",
                       class(self)[1], " for further assistance.")
        private$logR$log(method = 'nGrams', event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                               Get Method                                #
    #-------------------------------------------------------------------------#
    getTokens = function() private$..tokens,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$token(self)
    }
  )
)
