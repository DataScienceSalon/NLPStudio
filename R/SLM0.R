#==============================================================================#
#                                   SLM0                                       #
#==============================================================================#
#' SLM0
#'
#' \code{SLM0} Base class for the statistical learning family of classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @export
SLM0 <- R6::R6Class(
  classname = "SLM0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..settings = list(
      modelName = character(),
      modelSize = numeric(),
      openVocabulary = logical(),
      algorithm = character(),
      modelType = character(),
      modelTypes = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram')
    ),
    ..regex = list(
      tail = "^.* ([[:alnum:]]+)$",
      prefix = list(
        bigrams   = "^((\\S+\\s+){0}\\S+).*$",
        trigrams  = "^((\\S+\\s+){1}\\S+).*$",
        quadgrams = "^((\\S+\\s+){2}\\S+).*$",
        quintgrams = "^((\\S+\\s+){3}\\S+).*$"
      ),
      suffix = list(
        bigrams = "^.*\\s+((?:\\S+\\s+){0}\\S+)$",
        trigrams = "^.*\\s+((?:\\S+\\s+){1}\\S+)$",
        quadgrams = "^.*\\s+((?:\\S+\\s+){2}\\S+)$",
        quintgrams = "^.*\\s+((?:\\S+\\s+){3}\\S+)$"
      )
    ),
    ..corpora = list(
      train = character(),
      test = character(),
      vocabulary = character()
    ),
    ..corporaStats = list(
      train = list(
        vocabulary = numeric(),
        words = numeric(),
        sentences = numeric(),
        oov = numeric(),
        N = numeric()
      ),
      test = list(
        vocabulary = numeric(),
        words = numeric(),
        sentences = numeric(),
        oov = numeric(),
        N = numeric()
      )
    ),
    ..model = list(
      nGrams = list(),
      discounts = data.frame(),
      totals = data.frame()
    ),
    ..evaluation = list(
      scores = data.table(),
      performance = list(
        nGrams = numeric(),
        oov = 0,
        oovRate = numeric(),
        zeroProbs = numeric(),
        zeroProbRate = numeric(),
        logProb = numeric(),
        perplexity = numeric()
      )
    ),
    #-------------------------------------------------------------------------#
    #                           EVALUATION METHODS                            #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                           initScoresTables                              #
    #                     Initialize nGram scores table                       #
    #-------------------------------------------------------------------------#
    initScoresTable = function() {
      test <- private$..corpora$test
      size <- private$..settings$modelSize

      tokens <- Token$new(test)$nGrams('tokenizer', size)$getTokens()
      documents <- tokens$getDocuments()
      nGrams <- unlist(lapply(documents, function(d) {d$content}))

      private$..evaluation$scores <- data.table(n = seq(1:length(nGrams)),
                                                nGram = nGrams)
      return(TRUE)
    },

    prepEvalReport = function() {
      # Test set nGram count
      private$..evaluation$performance$nGrams <- nrow(private$..evaluation$scores)

      # Test set OOV Rate
      private$..evaluation$performance$oovRate <-
        private$..evaluation$performance$oov /
        private$..corporaStats$test$N

      # Number of zero probability nGrams and zero probability rate
      private$..evaluation$performance$zeroProbs <-
        nrow(subset(private$..evaluation$scores, p == 0))
      private$..evaluation$performance$zeroProbRate <-
        private$..evaluation$performance$zeroProbs /
        private$..evaluation$performance$nGrams

      # Total Log probability
      private$..evaluation$performance$logProb <-
        as.numeric(sum(private$..evaluation$scores %>% filter(p != 0) %>%
        mutate(logProb = log(p)) %>% select(logProb)))

      # Perplexity
      private$..evaluation$performance$perplexity <-
        2^(-private$..evaluation$performance$logProb /
             private$..corporaStats$test$N)
    },

    #-------------------------------------------------------------------------#
    #                             NGRAM TABLES                                #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                                 totals                                  #
    #                 Summarizes totals from nGram Tables                     #
    #-------------------------------------------------------------------------#
    # Note: Defaults to counting nGrams that start with one or several
    # start of sentence tags. Therefore, there total nGram counts are equal
    # across all nGrams.
    totals = function() {
      private$..model$totals <- rbindlist(lapply(seq_along(private$..model$nGrams), function (n) {
        df <- private$..model$nGrams[[n]] %>% filter(nGram != paste(rep('BOS',n), collapse = " "))
        nGramTypes <- data.frame(nGramTypes = nrow(df))
        nums <- unlist(lapply(df, is.numeric))
        df <- as.data.frame(df[,nums])
        total <- as.data.frame(colSums(df))
        names(total) <- names(private$..model$nGrams[[n]])[nums]
        total <- cbind(n, nGramTypes, total)
      }))
    },

    #-------------------------------------------------------------------------#
    #                           createTable                                   #
    #         Creates and initializes an nGram table with raw counts          #
    #-------------------------------------------------------------------------#
    createTable = function(nGrams, n) {
      nGrams <- as.data.frame(table(nGrams), stringsAsFactors = FALSE)
      dt <- data.table(nGram = nGrams[,1],
                       cNGram = nGrams[,2])

      dt <- dt[nGram != paste(rep("BOS", n), collapse = " ")]

      # Add prefix and suffix to nGram Table
      if (n > 1) {
        dt$prefix <- gsub(private$..regex$prefix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$suffix  <- gsub(private$..regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$tail  <- sub(private$..regex$tail, "\\1", dt$nGram, perl = TRUE)
      }

      return(dt)
    },

    #-------------------------------------------------------------------------#
    #                           initNGramTables                               #
    #            Initialize  nGram tables from the training text              #
    #-------------------------------------------------------------------------#
    initNGramTables = function() {

      # Obtain model parameters and training Corpus object.
      modelSize <- private$..settings$modelSize
      modelTypes <- private$..settings$modelTypes

      # Initialize Tables
      private$..model$nGrams <- list()
      private$..model$nGrams <- lapply(seq(1:modelSize), function(n) {
        corpus <- Token$new(private$..corpora$train)$nGrams('quanteda', n)$getTokens()
        documents <- corpus$getDocuments()
        nGrams <- unlist(lapply(documents, function(d) {d$content}))
        private$createTable(nGrams, n)
      })

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                        TEXT PROCESSING METHODS                          #
    #-------------------------------------------------------------------------#
    #-------------------------------------------------------------------------#
    #               Gsub - Replaces selected words with 'UNK'                 #
    #-------------------------------------------------------------------------#
    gsubq = function(text, words) {
      # Replaces OOV words with the 'UNK' pseudo-word in chunks of 1000 words

      size <- length(words) %% 1000
      wordChunks <- base::split(words, ceiling(seq_along(words)/size))
      for (i in 1:length(wordChunks)) {
        pattern <- paste0("\\b(?:", paste0(wordChunks[[i]], collapse = "|"), ")\\b ?")
        text <- gsub(pattern = pattern, replacement = " UNK ", text, perl = TRUE)
      }
      # Remove extra white space
      text <- gsub("\\s+"," ",text)
      return(text)
    },
    #-------------------------------------------------------------------------#
    #                               corpusStats                               #
    #         Final summary of corpus statistics for evaluation report        #
    #-------------------------------------------------------------------------#
    corpusStats = function(corpus) {

      stats <- list()
      documents <- corpus$getDocuments()
      content <- unlist(lapply(documents, function(d) { d$content }))

      stats$vocabulary <-
        length(unique(unlist(tokenizers::tokenize_words(content, strip_punct = FALSE, lowercase = FALSE))))
      stats$words <- sum(tokenizers::count_words(content))
      stats$sentences <- sum(tokenizers::count_sentences(content))
      stats$oov <- sum(grepl("UNK", content, fixed = TRUE, ignore.case = FALSE))
      stats$N <- stats$words + stats$sentences

      return(stats)
    },

    #-------------------------------------------------------------------------#
    #                               prepTrain                                 #
    #   Annotates Training Set with BOS/EOS and sets hapax legomena to 'UNK'  #
    #-------------------------------------------------------------------------#
    prepTrain = function() {

      # Extract vocabulary
      train <- Token$new(private$..corpora$train)$words('quanteda')$getTokens()
      documents <- train$getDocuments()
      tokens <- unname(unlist(lapply(documents, function(d) {d$content})))
      private$..corpora$vocabulary <- unique(tokens)

      # If open vocabulary, replace hapax legomena with the 'UNK' pseudo word
      if (private$..settings$openVocabulary) {

        # Create frequency table, identify hapax legomena and mark as UNK.
        freq <- as.data.frame(table(tokens), stringsAsFactors = FALSE)
        hapax  <- (freq %>% filter(Freq == 1) %>% select(tokens))$tokens

        # Replace hapax legomena in each document with the UNK pseudo word
        documents <- private$..corpora$train$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$gsubq(text = documents[[i]]$content,
                                                  words = hapax)
          private$..corpora$train$addDocument(documents[[i]])
        }

        # Remove hapax legomena from vocabulary
        private$..corpora$vocabulary <-
          private$..corpora$vocabulary[!private$..corpora$vocabulary %in% hapax]
      }

      # Finalize corpus statistics for evaluation report
      private$..corporaStats$train <-
        private$corpusStats(private$..corpora$train)

      # Annotate training corpus with sentence boundary tokens
      private$..corpora$train <- private$annotate(private$..corpora$train)

      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                               prepTest                                  #
    #   Annotates Training Set with BOS/EOS and closes Vocabulary by setting  #
    #   words not in training set to pseudo work 'UNK'                        #
    #-------------------------------------------------------------------------#
    prepTest = function() {

      # Extract vocabulary
      test <- Token$new(private$..corpora$test)$words('quanteda')$getTokens()
      documents <- test$getDocuments()
      testVocabulary <- unique(unlist(lapply(documents, function(d) {d$content})))
      oov <- unique(testVocabulary[!testVocabulary %fin% private$..corpora$vocabulary])

      # If open vocabulary replace any word not in vocabulary with 'UNK'token
      if (private$..settings$openVocabulary) {

        # Replace OOV words with UNK pseudo-word in each document
        documents <- private$..corpora$test$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$gsubq(text = documents[[i]]$content,
                                                  words = oov)
          private$..corpora$test$addDocument(documents[[i]])
        }
      }

      # Finalize corpus statistics for evaluation report
      private$..corporaStats$test <-
        private$corpusStats(private$..corpora$test)

      # Annotate test corpus with sentence boundary tokens
      private$..corpora$test <- private$annotate(private$..corpora$test)

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                               annotate                                  #
    #                  Annotates text with BOS/EOS tags                       #
    #-------------------------------------------------------------------------#
    annotate = function(corpus) {
      # Annotates text with appropriate start and end of sentence tokens.
      documents <- corpus$getDocuments()
      for (i in 1:length(documents)) {
        documents[[i]]$content <-
          paste(paste0(rep("BOS", times = private$..settings$modelSize-1), collapse = " "),
                documents[[i]]$content,
                "EOS", sep = " ")
        corpus$addDocument(documents[[i]])
      }
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()

      heading <- paste(private$..settings$modelName,
                       private$..settings$algorithm,
                       private$..settings$modelType, "Summary")


      NLPStudio::printHeading(text = heading, symbol = "=", newlines = 2)

      cat(paste0("\nId         : ", meta$identity$id))
      cat(paste0("\nName       : ", private$..settings$modelName))
      cat(paste0("\nType       : ", private$..settings$modelType))
      cat(paste0("\nAlgorithm  : ", private$..settings$algorithm))
      cat(paste0("\nVocabulary : ", private$..settings$openVocabulary))
      return(TRUE)

    },

    nGramSummary = function() {

      heading <- paste(private$..settings$modelName,
                       private$..settings$algorithm,
                       private$..settings$modelType, "nGram Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$totals)
      return(TRUE)

    },

    discountSummary = function() {

      heading <- paste(private$..settings$modelName,
                       private$..settings$algorithm,
                       private$..settings$modelType, "Discount Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$discounts)
      return(TRUE)

    },

    nGramDetail = function() {

      if (length(private$..model$nGrams) > 0) {

        for (i in 1:private$..settings$modelSize) {
          heading <- paste(private$..settings$modelName,
                           private$..settings$algorithm,
                           private$..settings$modelTypes[i], "Detail")
          NLPStudio::printHeading(text = heading,
                                  symbol = "-",
                                  newlines = 2)
          setkey(private$..model$nGrams[[i]], nGram)
          print(private$..model$nGrams[[i]][, tail(.SD, 10), by=nGram])
        }
      }
      return(TRUE)
    },

    evalSummary = function() {

      heading <- paste(private$..settings$modelName,
                       private$..settings$algorithm,
                       private$..settings$modelType, "Evaluation")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      cat("\nTraining Set Summary\n")
      print(as.data.frame(private$..corporaStats$train), row.names = FALSE)
      cat("\nTest Set Summary\n")
      print(as.data.frame(private$..corporaStats$test), row.names = FALSE)
      cat("\nPerformance Summary\n")
      print(as.data.frame(private$..evaluation$performance), row.names = FALSE)
      cat("\n")
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function() stop("This method is not implemented for this base class."),

    #-------------------------------------------------------------------------#
    #                          Accessor Methods                               #
    #-------------------------------------------------------------------------#
    getCorpora = function() private$..corpora,
    getModel = function() private$..model,
    getNGrams = function() private$..model$nGrams,
    getDiscounts = function() private$..model$discounts,
    getTotals = function() private$..model$totals,
    getScores = function() private$..evaluation$scores,
    getEval = function() {
      private$evalSummary()
      return(private$..evaluation)
    },

    #-------------------------------------------------------------------------#
    #                             Summary Method                              #
    #-------------------------------------------------------------------------#
    summary = function() {
      private$overview()
      if (length(private$..corpora$train) > 0) private$..corpora$train$summary()
      if (length(private$..corpora$test) > 0) private$..corpora$test$summary()
      if (length(private$..model$total) > 0) private$nGramSummary()
      if (length(private$..model$discounts) > 0) private$discountSummary()
      if (length(private$..model$nGrams) > 0) private$nGramDetail()
      if (length(private$..evaluation$performance$perplexity) > 0) private$evalSummary()
      invisible(self)
    }
  )
)
