#==============================================================================#
#                                SLMCorpora                                    #
#==============================================================================#
#' SLMCorpora
#'
#' \code{SLMCorpora} Class responsible for preparing training and test Corpora  objects for language modeling.
#'
#' Class responsible for preparing training Corpus objects for language modeling.
#'
#' @param config SLMConfig object containing the model parameters and the
#' training and test Corpus objects.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family SLMStudio Classes
#' @export
SLMCorpora <- R6::R6Class(
  classname = "SLMCorpora",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLMStudio0,

  private = list(

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
      return(text)
    },

    processTrain = function() {

      train <- private$..train$getDocuments()[[1]]

      tokens <- unlist(NLPStudio::tokenize(x = train$content, tokenUnit = 'word'))
      freq <- as.data.frame(table(tokens), stringsAsFactors = FALSE)
      hapax  <- (freq %>% filter(Freq == 1) %>% select(tokens))$tokens

      train$content <- private$gsubq(text = train$content, words = hapax)

      # Compute and update quantitative data
      netWords <- private$countWords(tokens = tokens)
      train$setMeta(key = 'netWords', value = netWords, type = 'q')
      train$setMeta(key = 'OOV', value = length(hapax), type = 'q')
      private$..train$setMeta(key = 'netWords', value = netWords, type = 'q')
      private$..train$setMeta(key = 'OOV', value = length(hapax), type = 'q')
      private$..train$addDocument(train)
      return(TRUE)
    },

    processTest = function() {

      # Assumes a single document Corpus
      train <- private$..train$getDocuments()[[1]]
      test <- private$..test$getDocuments()[[1]]

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
      private$..test$setMeta(key = 'netWords', value = netWords, type = 'q')
      private$..test$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..test$addDocument(test)
      return(TRUE)
    },

    annotate = function(corpus) {
      # Assumes a single document Corpus
      document <- corpus$getDocuments()[[1]]
      document$content <-
        paste(paste0(rep("BOS", times = private$..modelSize-1), collapse = " "),
              document$content,
              "EOS", sep = " ")

      corpus$purgeDocuments()
      corpus$addDocument(document)
      return(corpus)
    },

    prepTrain = function() {

      private$..train <- private$combine(private$..train)

      private$..train <- private$annotate(private$..train)

      if (private$..config$isOpen()) private$processTrain()

      invisible(self)
    },

    prepTest = function() {

      private$..test <- private$combine(private$..test)

      private$..test <- private$annotate(private$..test)

      if (private$..config$isOpen()) private$processTest()

      invisible(self)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(train, test, config) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('config')
      private$..params$classes$objects <- list(config)
      private$..params$classes$valid <- list('SLMConfig')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      # Dock statistical language model configuration and Corpus objects.
      private$..config <- config
      private$..train <- train
      private$..test <- test

      invisible(self)
    },

    build = function() {
      private$prepTrain()
      private$prepTest()
      invisible(self)
    },

    getTrain = function() { return(private$..train) },
    getTest = function() { return(private$..test) },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$slmCorpora(self)
    }
  )
)
