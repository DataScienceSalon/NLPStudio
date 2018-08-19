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
    ..tokenizedCorpus = character(),
    ..counts = data.frame(),
    ..tokenUnit = character(),

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
    #                           Execute Method                                #
    #-------------------------------------------------------------------------#
    execute = function() {

      # Format nGram Type
      tokenType <- private$..tokenUnit
      if (grepl("^n", private$..tokenUnit, ignore.case = TRUE)) {
        if (n == 1)  tokenType <- "Unigrams"
        if (n == 2)  tokenType <- "Bigrams"
        if (n == 3)  tokenType <- "Trigrams"
        if (n == 4)  tokenType <- "Quadgrams"
        if (n == 5)  tokenType <- "Quintgrams"
      }
      # Create tokens object and obtain documents fro
      private$..tokenizedCorpus <- Clone$new()$this(private$..x, reference = FALSE,
                                           content = FALSE)
      documents <- private$..x$getDocuments()

      # Prepare for parallel processing
      numCores <- detectCores()
      cl <- makeCluster(numCores-1)
      clusterExport(cl, varlist = c("documents", "Clone", "private"), envir = environment())
      junk <- clusterEvalQ(cl, library(tokenizers))
      junk <- clusterEvalQ(cl, library(futile.logger))

      docs <- parallel::parLapply(cl, seq(1:length(documents)), function(i) {

         #-----------------------------------------------------------------------#
         #                           Tokenize Function                           #
         #-----------------------------------------------------------------------#
         tokenize = function(content) {

           if (grepl("^c", private$..tokenUnit, ignore.case = TRUE)) {
             return(unlist(tokenizers::tokenize_characters(x = content,
                                                           lowercase = FALSE,
                                                           strip_non_alphanum = FALSE,
                                                           simplify = FALSE)))
           } else if (grepl("^w", private$..tokenUnit, ignore.case = TRUE)) {
             return(unlist(tokenizers::tokenize_words(x = content,
                                                      lowercase = FALSE,
                                                      strip_punct = FALSE,
                                                      strip_numeric = FALSE,
                                                      simplify = FALSE)))
           } else if (grepl("^s", private$..tokenUnit, ignore.case = TRUE)) {
             return(unlist(tokenizers::tokenize_sentences(x = content,
                                                          lowercase = FALSE,
                                                          strip_punct = FALSE,
                                                          simplify = FALSE)))
           } else if (grepl("^p", private$..tokenUnit, ignore.case = TRUE)) {
             return(unlist(tokenizers::tokenize_paragraphs(x = content,
                                                           paragraph_break = private$..paragraphBreak,
                                                           simplify = FALSE)))
           } else if (grepl("^n", private$..tokenUnit, ignore.case = TRUE)) {
             return(unlist(tokenizers::tokenize_ngrams(x = content, lowercase = FALSE,
                                                       n = private$..n, n_min = private$..n,
                                                       stopwords = private$..stopwords,
                                                       ngram_delim = private$..nGramDelim,
                                                       simplify = FALSE)))
           } else {
             event <- paste0("Invalid token unit. Must be c('character', 'word',",
                             " 'sentence', 'paragraph'). See ?", class(self)[1],
                             " for further assistance.")
             private$logR$log(method = 'tokenize', event = event, level = "Error")
             stop()
           }
         }

        # Clone and tokenize document
        document <- Clone$new()$this(documents[[i]],  content = TRUE)
        document$content <- unlist(lapply(document$content, function(d){ tokenize(d)}))
        document
      })
      stopCluster(cl)

      # Compute counts
      private$..counts <- rbindlist(lapply(docs, function(d) {
        document <- d$getName()
        terms <- length(unlist(d$content))
        types <- length(unique(unlist(d$content)))
        counts <- data.frame(document = document,
                             terms = terms,
                             types = types, stringsAsFactors = FALSE)
        counts
      }))
      types <- length(unique(unlist(lapply(docs, function(d) {d$content}))))
      counts <- data.frame(document = "Total",
                           terms = sum(private$..counts$terms),
                           types = types)
      private$..counts <- rbind(private$..counts, counts)


      # Create tokenized corpus object and add documents
      private$..tokenizedCorpus <- Clone$new()$this(private$..x, reference = FALSE,
                                                    content = FALSE)
      for (i in 1:length(docs)) {
        private$..tokenizedCorpus$addDocument(docs[[i]])
      }

      # Update corpus metadata
      name <- private$..tokenizedCorpus$getName()
      name <- paste0(name, " (", tokenType," Tokens)")
      private$..tokenizedCorpus$setName(name)

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
      private$..tokenUnit <- 'character'

      private$execute()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Word Tokens Method                            #
    #-------------------------------------------------------------------------#
    words = function(x) {

      private$validate(x)
      private$..x <- x
      private$..tokenUnit <- 'word'
      private$execute()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Sentence Tokens Method                            #
    #-------------------------------------------------------------------------#
    sentences = function(x) {

      private$validate(x)
      private$..x <- x
      private$..tokenUnit <- 'sentence'
      private$execute()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Paragraph Tokens Method                           #
    #-------------------------------------------------------------------------#
    paragraphs = function(x, paragraphBreak = "\n\n") {

      private$validate(x)
      private$..x <- x
      private$..tokenUnit <- 'Paragraph'
      private$..paragraphBreak <- paragraphBreak
      private$execute()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            NGrams Method                                #
    #-------------------------------------------------------------------------#
    nGrams = function(x, n = 3, stopwords = character(), nGramDelim = " ") {

      private$validate(x)
      private$..x <- x
      private$..tokenUnit <- 'nGram'
      private$..n <- n
      private$..stopwords <- stopwords
      private$..nGramDelim <- nGramDelim
      private$execute()

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
      visitor$tokenizer(self)
    }
  )
)
