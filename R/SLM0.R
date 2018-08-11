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
    ..parameters = list(
      modelId = character(),
      modelName = character(),
      algorithm = character(),
      modelSize = numeric(),
      modelType = character(),
      openVocabulary = logical(),
      vocabulary = character(),
      bosTags = logical()
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
    ..eval = list(
      nGrams = list(),
      timing = list(
        train = list(
          process = character(),
          start = character(),
          end = character(),
          duration = character()
        ),
        evaluation = list(
          process = character(),
          start = character(),
          end = character(),
          duration = character()
        )
      ),
      score = list(
        nGrams = numeric(),
        oov = 0,
        oovRate = numeric(),
        zeroProbs = numeric(),
        zeroProbRate = numeric(),
        logProb = numeric(),
        perplexity = numeric()
      )
    ),
    ..constants = list(
      modelTypes = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),
      regex = list(
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
      )
    ),
    #=========================================================================#
    #                         LANGUAGE MODEL TABLES                           #
    #=========================================================================#

    #-------------------------------------------------------------------------#
    #                           computeTotals                                 #
    #                 Summarizes totals from nGram Tables                     #
    #-------------------------------------------------------------------------#
    # Note: Defaults to counting nGrams that start with one or several
    # start of sentence tags. Therefore, there total nGram counts are equal
    # across all nGrams.
    computeTotals = function() {
      private$..model$totals <- rbindlist(lapply(seq_along(private$..model$nGrams), function (n) {
        df <- private$..model$nGrams[[n]] %>% filter(nGram != paste(rep('BOS',n), collapse = " "))
        nGramTypes <- data.frame(nGramTypes = nrow(df))
        nums <- unlist(lapply(df, is.numeric))
        df <- as.data.frame(df[,..nums])
        total <- as.data.frame(colSums(df))
        names(total) <- names(private$..model$nGrams[[n]])[nums]
        total <- cbind(n, nGramTypes, total)
      }))
      return(TRUE)
    },

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
    #                               computeCorpusStats                        #
    #         Final summary of corpus statistics for evaluation report        #
    #-------------------------------------------------------------------------#
    computeCorpusStats = function(corpus) {

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
      train <- Token$new()$words(x = private$..corpora$train, 'quanteda')$getCorpus()
      documents <- train$getDocuments()
      tokens <- unname(unlist(lapply(documents, function(d) {d$content})))
      private$..corpora$vocabulary <- unique(tokens)

      # If open vocabulary, replace hapax legomena with the 'UNK' pseudo word
      if (private$..parameters$vocabulary == 'Open') {

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
        private$computeCorpusStats(private$..corpora$train)

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
      test <- Token$new()$words(x = private$..corpora$test, 'quanteda')$getCorpus()
      documents <- test$getDocuments()
      testVocabulary <- unique(unlist(lapply(documents, function(d) {d$content})))
      oov <- unique(testVocabulary[!testVocabulary %fin% private$..corpora$vocabulary])
      private$..eval$score$oov <- length(oov)

      # If open vocabulary replace any word not in vocabulary with 'UNK'token
      if (private$..parameters$vocabulary == 'Open') {

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
        private$computeCorpusStats(private$..corpora$test)

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
        if (private$..parameters$bosTags) {
          documents[[i]]$content <-
            paste(paste0(rep("BOS", times = private$..parameters$modelSize-1), collapse = " "),
                  documents[[i]]$content,
                  "EOS", sep = " ")
        } else {
          documents[[i]]$content <-
            paste(documents[[i]]$content, "EOS", sep = " ")
        }

        corpus$addDocument(documents[[i]])
      }
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                             initModel                                   #
    #           Initialize nGram tables from the training corpus              #
    #-------------------------------------------------------------------------#
    initModel = function() {

      # Initialize and obtain model parameters and training Corpus object.
      modelSize <- private$..parameters$modelSize
      modelTypes <- private$..constants$modelTypes[1:private$..parameters$modelSize]

      # Tokenize corpus into nGrams of order n, and compute counts
      private$..model$nGrams <- lapply(seq(1:modelSize), function(n) {

        nGrams <- Token$new()$nGrams(x = private$..corpora$train, tokenizer = 'quanteda', n = n)$getNGrams()
        nGrams <- as.data.frame(table(nGrams), stringsAsFactors = FALSE)

        dt <- data.table(nGram = nGrams[,1], cNGram = nGrams[,2])
        dt$cNGram <- ifelse(dt$nGram == paste(rep("BOS", n), collapse = " "),
                            private$..corporaStats$train$sentences,
                            dt$cNGram)

        if (n > 1) {
          dt$prefix <- gsub(private$..constants$regex$prefix[[n-1]], "\\1", dt$nGram, perl = TRUE)
          dt$suffix  <- gsub(private$..constants$regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        }
        dt
      })

      names(private$..model$nGrams) <- modelTypes
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             initEvalNGrams                              #
    #         Initialize evaluation nGram tables from the test corpus         #
    #-------------------------------------------------------------------------#
    initEvalNGrams = function() {

      modelSize <- private$..parameters$modelSize
      modelTypes <- private$..constants$modelTypes[1:private$..parameters$modelSize]

      # Tokenize corpus into nGrams of order n and create nGram table
      private$..eval$nGrams <- lapply(seq(1:modelSize), function(n) {

        # Obtain nGrams and create table
        nGrams <- Token$new()$nGrams(x = private$..corpora$test,
                                     tokenizer = 'quanteda', n = n)$getNGrams()
        dt <- data.table(nGram = unique(nGrams))

        # Obtain counts from model and add to eval nGram table
        cNGram <- private$..model$nGrams[[n]] %>% select(nGram, cNGram)
        dt <- merge(dt, cNGram, by = 'nGram', all.x = TRUE)

        # Add prefix and suffix to nGram Table
        if (n > 1) {
          dt$prefix <- gsub(private$..constants$regex$prefix[[n-1]], "\\1", dt$nGram, perl = TRUE)
          dt$suffix  <- gsub(private$..constants$regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        }
        dt
      })

      names(private$..eval$nGrams) <- modelTypes
      return(TRUE)
    },


    #=========================================================================#
    #                       SUMMARY AND REPORTING METHODS                     #
    #=========================================================================#
    #-------------------------------------------------------------------------#
    #                         printOverview Method                            #
    #                 Summarizes the model and parameters                     #
    #-------------------------------------------------------------------------#
    printOverview = function() {

      meta <- private$meta$get()

      heading <- paste(private$..parameters$modelName, "Summary")


      NLPStudio::printHeading(text = heading, symbol = "=", newlines = 2)

      cat(paste0("\nId            : ", meta$identity$id))
      cat(paste0("\nName          : ", private$..parameters$modelName))
      cat(paste0("\nType          : ", private$..parameters$modelType))
      cat(paste0("\nAlgorithm     : ", private$..parameters$algorithm))
      cat(paste0("\nVocabulary    : ", private$..parameters$vocabulary))
      cat(paste0("\nModel <s> tags: ", private$..parameters$bosTags))
      return(TRUE)

    },

    #-------------------------------------------------------------------------#
    #                         printNGramSummary                               #
    #                       Summarizes ngGram Totals                          #
    #-------------------------------------------------------------------------#
    printNGramSummary = function() {

      heading <- paste(private$..parameters$modelName, "nGram Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$totals)
      return(TRUE)

    },

    #-------------------------------------------------------------------------#
    #                           printDiscountSummary                               #
    #                        Summarizes Discounts                             #
    #-------------------------------------------------------------------------#
    printDiscountSummary = function() {

      heading <- paste(private$..parameters$modelName, "Discount Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$discounts)
      return(TRUE)

    },


    #-------------------------------------------------------------------------#
    #                                timing                                   #
    #                 Captures process start and end times                    #
    #-------------------------------------------------------------------------#
    startTime = function(train = TRUE) {
      if (train) {
        private$..eval$timing$train$process <- "Training"
        private$..eval$timing$train$start <- as.character(Sys.time())
      } else {
        private$..eval$timing$evaluation$process <- "Evaluation"
        private$..eval$timing$evaluation$start <- as.character(Sys.time())
      }
    },

    endTime = function(train = TRUE) {
      if (train) {
        private$..eval$timing$train$end <- as.character(Sys.time())
        private$..eval$timing$train$duration <- as.character(format(difftime(
          private$..eval$timing$train$end,
          private$..eval$timing$train$start, units = "auto")))
      } else {
        private$..eval$timing$evaluation$end <- as.character(Sys.time())
        private$..eval$timing$evaluation$duration <- as.character(format(difftime(
          private$..eval$timing$evaluation$end,
          private$..eval$timing$evaluation$start, units = "auto")))
      }
    },

    #-------------------------------------------------------------------------#
    #                           prepEvalReport                                #
    # Computes OOV Rates, Zero Probabilities Log probabilities and Perplexity #
    #-------------------------------------------------------------------------#
    prepEvalReport = function() {
      # Test set nGram count
      private$..eval$score$nGrams <- nrow(private$..eval$scores)

      # Test set OOV Rate
      private$..eval$score$oovRate <-
        private$..eval$score$oov /
        private$..corporaStats$test$N

      # Number of zero probability nGrams and zero probability rate
      private$..eval$score$zeroProbs <-
        nrow(subset(private$..eval$scores, p == 0))
      private$..eval$score$zeroProbRate <-
        private$..eval$score$zeroProbs /
        private$..eval$score$nGrams

      # Total Log probability
      private$..eval$score$logProb <-
        as.numeric(sum(private$..eval$scores %>% filter(p != 0) %>%
                         mutate(logProb = log(p)) %>% select(logProb)))

      # Perplexity
      private$..eval$score$perplexity <-
        2^(-private$..eval$score$logProb /
             private$..corporaStats$test$N)
    },

    #-------------------------------------------------------------------------#
    #                           printEvalSummary                              #
    #                    Prints evaluation summary                            #
    #-------------------------------------------------------------------------#
    printEvalSummary = function() {

      heading <- paste(private$..parameters$modelName, "Evaluation")
      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)

      cat("\nData Summary\n")
      train <- as.data.frame(private$..corporaStats$train)
      test <- as.data.frame(private$..corporaStats$test)
      dataSummary <- rbind(train, test)
      dataSummary <- cbind(data.frame(data = names(private$..corporaStats)), dataSummary)
      print(dataSummary, row.names = FALSE)

      cat("\nTiming Summary\n")
      timings <- rbindlist(private$..eval$timing, fill = TRUE)
      print(timings, row.names = FALSE)

      cat("\nPerformance Summary\n")
      print(as.data.frame(private$..eval$score), row.names = FALSE)
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
    getModelNGrams = function() private$..model$nGrams,
    getDiscounts = function() private$..model$discounts,
    getTotals = function() private$..model$totals,
    getEvalNGrams = function() private$..eval$nGrams,
    getScores = function() private$..eval$scores,

    #-------------------------------------------------------------------------#
    #                             Summary Method                              #
    #-------------------------------------------------------------------------#
    summary = function() {
      private$printOverview()
      if (length(private$..model$total) > 0) private$printNGramSummary()
      if (length(private$..model$discounts) > 0) private$printDiscountSummary()
      if (length(private$..eval$score$perplexity) > 0) private$printEvalSummary()
      invisible(self)
    }
  )
)
