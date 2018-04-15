#------------------------------------------------------------------------------#
#                                  Tokenizer                                   #
#------------------------------------------------------------------------------#
#' Tokenizer
#'
#' \code{Tokenizer}  Creates tokenized corpus objects.
#'
#' This class is a wrapper for the  \code{\link[quanteda]{tokens}} function for
#' character, and word, tokenization. Sentence tokenization functionality is
#' provided using the openNLP package.
#'
#' Sources:
#' \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#' \url{https://cran.r-project.org/web/packages/openNLP/openNLP.pdf}
#'
#' @usage Tokenizer$new(x, what = c("sentence", "word"))$execute()
#'
#' @param what Character string containing either c('character', 'word' ,'sentence)
#' indicating to which format the document should be tokenized.
#'
#' @examples
#'
#' @return \code{Tokenizer} A tokenized Corpus, Document, or character text
#' object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @family Tokens Classes
#' @export
Tokenizer <- R6::R6Class(
  classname = "Tokenizer",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..what = character(),
    ..tokens = character(),

    processDocument = function(document) {

      # Obtain metadata and content of TextDocument object
      content <- document$content

      # Produce TokensDocument object content
      if (private$..what %in% c("sentence")) {

        # Use sentence token from openNLP and NLP packages
        s <- paste(content, collapse = "")
        s <- NLP::as.String(s)
        sa <- openNLP::Maxent_Sent_Token_Annotator()
        a <- NLP::annotate(s, sa)
        tokenized <- s[a]

      } else {
        tokenized <- quanteda::tokens(x = content,
                                      what = private$..what)
      }


      return(tokenized)
    },

    tokenizeCorpus = function() {

      id <- private$..x$getId()
      name <- private$..x$getName()

      # Add metadata to  tokens
      private$..tokens$setMeta(key = 'name', value = paste0(name, "(tokens)"))
      private$..tokens$setMeta(key = 'description',
                               value = paste0(name, " Corpus object in ",
                                              private$..what, " token format."))
      private$..tokens$setMeta(key = "sourceCorpusId", value = id, type = 'f')
      private$..tokens$setMeta(key = "sourceCorpusName", value = name, type = 'f')
      private$..tokens$setMeta(key = "tokensType", value = private$..what, type = 'f')

      # Create tokenized documents
      docs <- private$..x$getDocument()
      for (i in 1:length(docs)) {
        tokens <- private$processDocument(docs[[i]])
        names(tokens) <- docs[[i]]$getName()
        private$..tokens$add(tokens)
      }

      # Count tokens
      tokens <- 0
      docs <- private$..tokens$get()
      for (i in 1:length(docs)) {
        tokens <- tokens + length(docs[[i]])
      }
      private$..tokens$setMeta(key = private$..what, value = tokens, type = 'quant')

      # Log it
      event <- paste0("Created ", private$..what, "Tokens object ",
                      private$..tokens$getName(), ". ")
      private$..tokens$message(event = event)
      private$..x$message(event = event)
      private$logR$log(method = 'tokenizeCorpus', event = event)

      return(TRUE)
    }
  ),

  public = list(
    initialize = function() {

      private$loadDependencies()
      private$..tokens <- Tokens$new()

      invisible(self)
    },

    word = function(x) {
      private$..x <- x
      private$..what <- 'word'
      private$tokenizeCorpus()
      return(private$..tokens)
    },

    sentence = function(x) {
      private$..x <- x
      private$..what <- 'sentence'
      private$tokenizeCorpus()
      return(private$..tokens)
    },

    char = function(x) {
      private$..x <- x
      private$..what <- 'character'
      private$tokenizeCorpus()
      return(private$..tokens)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenize(self)
    }
  )
)
