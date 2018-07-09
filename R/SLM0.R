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
    ..model = list(
      nGrams = list(),
      discounts = data.frame(),
      totals = data.frame()
    ),
    ..evaluation = list(
      scores = data.table(),
      summary = list(
        trainVocabulary = numeric(),
        testVocabulary = numeric(),
        oov = numeric(),
        oovRate = numeric(),
        exact = numeric(),
        exactRate = numeric(),
        zeroProbs = numeric(),
        zeroProbRate = numeric(),
        score = numeric(),
        logScore = numeric(),
        entropy = numeric(),
        perplexity = numeric()
      )
    ),

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

      if (!private$..settings$bos) {
        dt <- dt %>% filter(!grepl("BOS", nGram))
      }

      # Add prefix and suffix to nGram Table
      if (n > 1) {
        dt$prefix <- gsub(private$..regex$prefix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$suffix  <- gsub(private$..regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$tail  <- sub(private$..regex$tail, "\\1", dt$nGram, perl = TRUE)
      }

      return(dt)
    },

    #-------------------------------------------------------------------------#
    #                           initTrainTables                               #
    #            Initialize  nGram tables from the training text              #
    #-------------------------------------------------------------------------#
    initTrainTables = function() {

      # Obtain model parameters and training Corpus object.
      modelSize <- private$..settings$modelSize
      modelTypes <- private$..settings$modelTypes
      train <- private$..corpora$train

      # Initialize Tables
      private$..model$nGrams <- list()
      private$..model$nGrams <- lapply(seq(1:modelSize), function(n) {
        corpus <- Token$new(train)$nGrams('tokenizer', n)$getTokens()
        documents <- corpus$getDocuments()
        nGrams <- unlist(lapply(documents, function(d) {d$content}))
        private$createTable(nGrams, n)
      })

      private$totals()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                            initTestTables                               #
    #            Initialize  nGram tables from the test text                  #
    #-------------------------------------------------------------------------#
    initTestTable = function() {
      test <- private$..corpora$test
      size <- private$..settings$modelSize

      tokens <- Token$new(test)$nGrams('tokenizer', size)$getTokens()
      documents <- tokens$getDocuments()
      nGrams <- unlist(lapply(documents, function(d) {d$content}))

      private$..evaluation$scores <- data.table(n = seq(1:length(nGrams)),
                                                nGram = nGrams)
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
        pattern <- paste("\\b(?:", paste(wordChunks[[i]], collapse = "|"), ")\\b ?")
        text <- gsub(pattern = pattern, replacement = " UNK ", text, perl = TRUE)
      }
      # Remove extra white space
      text <- gsub("\\s+"," ",text)
      return(text)
    },
    #-------------------------------------------------------------------------#
    #                               prepTrain                                 #
    #   Annotates Training Set with BOS/EOS and sets hapax legomena to 'UNK'  #
    #-------------------------------------------------------------------------#
    prepTrain = function() {

      # Extract vocabulary
      train <- Token$new(private$..corpora$train)$words('tokenizer')$getTokens()
      documents <- train$getDocuments()
      tokens <- unlist(lapply(documents, function(d) {d$content}))
      trainVocabulary <- unique(tokens)

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
        private$..corpora$train$setMeta(key = 'OOV', value = length(hapax), type = 'q')
        private$..corpora$test$setMeta(key = 'OOV Rate',
                                       value = length(hapax) / length(trainVocabulary), type = 'q')
      }

      # Annotate training corpus with sentence boundary tokens
      private$..corpora$train <- private$annotate(private$..corpora$train)

      # Update corpus settings and evaluation statistics
      private$..corpora$vocabulary <- trainVocabulary
      private$..evaluation$summary$trainVocabulary <- length(trainVocabulary)

      # Update metadata
      private$..corpora$test$setMeta(key = 'Vocabulary', value = length(traintVocabulary), type = 'q')

      return()
    },
    #-------------------------------------------------------------------------#
    #                               prepTest                                  #
    #   Annotates Training Set with BOS/EOS and closes Vocabulary by setting  #
    #   words not in training set to pseudo work 'UNK'                        #
    #-------------------------------------------------------------------------#
    prepTest = function() {

      # Extract vocabulary
      test <- Token$new(private$..corpora$test)$words('tokenizer')$getTokens()
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

      # Annotate test corpus with sentence boundary tokens
      private$..corpora$test <- private$annotate(private$..corpora$test)

      # Update evaluation statistics
      private$..evaluation$summary$testVocabulary <- length(testVocabulary)
      private$..evaluation$summary$oov <- length(oov)
      private$..evaluation$summary$oovRate <- length(oov) / length(testVocabulary)

      # Update metadata
      private$..corpora$test$setMeta(key = 'Vocabulary', value = length(testVocabulary), type = 'q')
      private$..corpora$test$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..corpora$test$setMeta(key = 'OOV Rate', value = length(oov) / length(testVocabulary), type = 'q')
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
      print(private$..evaluation)
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
    getConfig = function() private$..config,
    getCorpora = function() private$..corpora,
    getModel = function() private$..model,
    getNGrams = function() private$..model$nGrams,
    getDiscounts = function() private$..model$discounts,
    getTotals = function() private$..model$totals,
    getScores = function() private$..scores,
    getEval = function() private$..evaluation,

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
      if (length(private$..evaluation) > 0) private$evalSummary()
      invisible(self)
    }
  )
)
