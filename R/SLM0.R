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
      test = character()
    ),
    ..model = list(
      discounts = data.table(),
      totals = data.table(),
      nGrams = list()
    ),
    ..scores = data.table(),
    ..evaluation = data.frame(),

    #-------------------------------------------------------------------------#
    #                       Corpus Preparation Methods                        #
    #-------------------------------------------------------------------------#
    combine = function(corpus) {

      # Creates a new Corpus object that will contain a single combined
      # Document object.

      # Create Clone Corpus
      modelCorpus <- Clone$new()$this(x = corpus, reference = FALSE)

      # Combine and extract corpus text
      documents <- corpus$getDocuments()
      text <- unname(unlist(lapply(documents, function(d) { d$content })))

      # Create new Document object and copy metadata from the Corpus object
      document <- Document$new(x = text)
      document <- Copy$new()$this(x = corpus, to = document)

      # Set metadata
      name <- corpus$getName()
      name <- paste0(name, " Document")
      document$setName(name)

      # Add document to modelCorpus object
      modelCorpus$addDocument(document)
      return(modelCorpus)
    },

    countWords = function(tokens) {
      # Computes net words = total words - BOS tokens
      bosTokens <- sum(grepl("BOS", tokens, ignore.case = FALSE, fixed = TRUE))
      netWords <- length(tokens) - bosTokens
      return(netWords)

    },

    gsubq = function(text, words) {
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

    processTrain = function() {

      train <- private$..corpora$train$getDocuments()[[1]]

      tokens <- unlist(NLPStudio::tokenize(x = train$content, tokenUnit = 'word'))
      freq <- as.data.frame(table(tokens), stringsAsFactors = FALSE)
      hapax  <- (freq %>% filter(Freq == 1) %>% select(tokens))$tokens

      train$content <- private$gsubq(text = train$content, words = hapax)

      # Compute and update quantitative data
      netWords <- private$countWords(tokens = tokens)
      train$setMeta(key = 'netWords', value = netWords, type = 'q')
      train$setMeta(key = 'OOV', value = length(hapax), type = 'q')
      private$..corpora$train$setMeta(key = 'netWords', value = netWords, type = 'q')
      private$..corpora$train$setMeta(key = 'OOV', value = length(hapax), type = 'q')
      private$..corpora$train$addDocument(train)
      return(TRUE)
    },

    processTest = function() {

      # Assumes a single document Corpus
      train <- private$..corpora$train$getDocuments()[[1]]
      test <- private$..corpora$test$getDocuments()[[1]]

      trainTokens <- unlist(NLPStudio::tokenize(x = train$content, tokenUnit = 'word'))
      vocabulary <- unique(trainTokens)
      testTokens <- unlist(NLPStudio::tokenize(x = test$content, tokenUnit = 'word'))
      testVocabulary <- unique(testTokens)
      oov <- unique(testVocabulary[!testVocabulary %fin% vocabulary])

      test$content <- private$gsubq(text = test$content, words = oov)

      # Compute and update quantitative data
      netWords <- private$countWords(tokens = testTokens)
      test$setMeta(key = 'netWords', value = netWords, type = 'q')
      test$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..corpora$test$setMeta(key = 'netWords', value = netWords, type = 'q')
      private$..corpora$test$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..corpora$test$addDocument(test)
      return(TRUE)
    },

    annotate = function(corpus) {
      # Annotates text with appropriate start and end of sentence tokens.
      document <- corpus$getDocuments()[[1]]
      document$content <-
        paste(paste0(rep("BOS", times = private$..settings$modelSize-1), collapse = " "),
              document$content,
              "EOS", sep = " ")

      corpus$addDocument(document)
      return(corpus)
    },

    prepTrain = function() {

      private$..corpora$train <- private$combine(private$..corpora$train)

      private$..corpora$train <- private$annotate(private$..corpora$train)

      if (private$..settings$openVocabulary) private$processTrain()

      invisible(self)
    },

    prepTest = function() {

      private$..corpora$test <- private$combine(private$..corpora$test)

      private$..corpora$test <- private$annotate(private$..corpora$test)

      if (private$..settings$openVocabulary) private$processTest()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Evaluation Methods                            #
    #-------------------------------------------------------------------------#
    score = function() {

      # Obtain networds and subtract zero probability words
      netWords <- private$..corpora$test$getMeta(key = 'netWords')
      OOV = private$..corpora$test$getMeta(key = 'OOV')
      exact <- nrow(private$..scores[ Match == private$..settings$modelSize])
      exactRate <- exact / nrow(private$..scores)
      zeroProbs <- nrow(private$..scores[ pSmooth == 0])
      scores <- private$..scores[ pSmooth > 0]
      scores[, logProb := log2(pSmooth)]
      H <- -1 * 1/netWords * sum(scores$logProb)
      pp <- 2^H

      private$..evaluation <- data.frame(N = netWords,
                                         OOV = OOV,
                                         OOV.Rate = OOV / netWords,
                                         ZeroProbs = zeroProbs,
                                         Exact = exact,
                                         ExactRate = exactRate,
                                         Entropy = H,
                                         Perplexity = pp)
      return(TRUE)
    },

    computeProbs = function() {

      i <- private$..settings$modelSize

      while (i > 0) {
        # Obtain training probabilities
        setkey(private$..model$nGrams[[i]], nGram)
        probs <- private$..model$nGrams[[i]][,.(nGram, pSmooth, Match = i)]

        if (i == 1) {
          setkey(private$..scores, Unigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Unigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pSmooth.update) & (Match == 0)),
                           c("Match", "pSmooth") := list(Match.update, pSmooth.update)]
          private$..scores[, c('pSmooth.update', 'Match.update') := NULL]
        } else if (i == 2) {
          setkey(private$..scores, Bigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Bigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pSmooth.update) & (Match == 0)),
                           c("Match", "pSmooth") := list(Match.update, pSmooth.update)]
          private$..scores[, c('pSmooth.update', 'Match.update') := NULL]

        } else if (i == 3) {
          setkey(private$..scores, Trigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Trigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pSmooth.update) & (Match == 0)),
                           c("Match", "pSmooth") := list(Match.update, pSmooth.update)]
          private$..scores[, c('pSmooth.update', 'Match.update') := NULL]

        } else if (i == 4) {
          setkey(private$..scores, Quadgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quadgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pSmooth.update) & (Match == 0)),
                           c("Match", "pSmooth") := list(Match.update, pSmooth.update)]
          private$..scores[, c('pSmooth.update', 'Match.update') := NULL]
        } else if (i == 5) {
          setkey(private$..scores, Quintgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quintgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pSmooth.update) & (Match == 0)),
                           c("Match", "pSmooth") := list(Match.update, pSmooth.update)]
          private$..scores[, c('pSmooth.update', 'Match.update') := NULL]
        }
        i <- i - 1
      }

    },

    buildTestNGrams = function() {
      document <- private$..corpora$test$getDocuments()[[1]]
      nGrams <- unlist(NLPStudio::tokenize(document$content, tokenUnit = 'word',
                                           nGrams = private$..settings$modelSize,
                                           lowercase = FALSE))
      nBackOff <- 1
      private$..scores <- data.table(nGrams = nGrams)
      while (nBackOff < private$..settings$modelSize) {
        nGrams <- gsub(pattern = "^([\\w'\\-]+) ",
                       replacement = "",x = nGrams, perl = TRUE)
        private$..scores <- cbind(private$..scores, nGrams)
        nBackOff <- nBackOff + 1
      }
      modelTypes <- private$..settings$modelTypes[1:private$..settings$modelSize]
      modelTypes <- rev(modelTypes)
      names(private$..scores) <- modelTypes[seq(1:private$..settings$modelSize)]
      private$..scores <- cbind(private$..scores, Match = 0, pSmooth = 0)
      return(TRUE)
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

    },


    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    validateClass = function(x, fieldName, className, methodName) {

      private$..params <- list()
      private$..params$classes$name <- list(fieldName)
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(className)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        return(FALSE)
      } else {
        return(TRUE)
      }
    },

    validateParams = function(train, test, name, modelSize, openVocabulary) {

      private$..params <- list()
      private$..params$classes$name <- list('train', 'test')
      private$..params$classes$objects <- list(train, test)
      private$..params$classes$valid <- list('Corpus', 'Corpus')
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      private$..params$logicals$variables <- c('openVocabulary')
      private$..params$logicals$values <- c(openVocabulary)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function() stop("This method is not implemented for this base class."),
    #-------------------------------------------------------------------------#
    #                             Evaluate Model                              #
    #-------------------------------------------------------------------------#
    evaluate = function() {

      private$buildTestNGrams()
      private$computeProbs()
      private$score()
      invisible(self)
    },

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
