#==============================================================================#
#                             LMCorpora                                #
#==============================================================================#
#' LMCorpora
#'
#' \code{LMCorpora} Class responsible for preparing training Corpus objects for language modeling.
#'
#' Class responsible for preparing training Corpus objects for language modeling.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family LMStudio Classes
#' @export
LMCorpora <- R6::R6Class(
  classname = "LMCorpora",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..train = character(),
    ..test = character(),
    ..modelSize = numeric(),
    ..openVocabulary = logical(),

    combine = function(corpus) {
      # combines a corpus with multiple documents into a corpus with a
      # single document.

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

      # Purge existing Document objects and add combined Document
      corpus$purgeDocuments()
      corpus$addDocument(document)
      return(corpus)
    },

    processTrainOOV = function() {

      document <- private$..train$getDocuments()[[1]]

      tokens <- unlist(NLPStudio::tokenize(x = document$content, tokenType = 'word'))
      freq <- as.data.frame(table(tokens), stringsAsFactors = FALSE)
      hapax  <- (freq %>% filter(Freq == 1) %>% select(tokens))
      document$content <-
        stringi::stri_replace_all_regex(str = document$content,
                                        pattern = paste0('\\b', hapax, '\\b'),
                                        replacement = 'UNK',
                                        vectorize_all = FALSE)

      document$setMeta(key = 'OOV', value = nrow(hapax), type = 'q')
      private$..train$setMeta(key = 'OOV', value = nrow(hapax), type = 'q')
      private$..train$addDocument(document)
      return(TRUE)
    },

    processTestOOV = function() {

      # Assumes a single document Corpus
      train <- private$..train$getDocuments()[[1]]
      test <- private$..test$getDocuments()[[1]]

      vocabulary <- unique(unlist(NLPStudio::tokenize(x = train$content, tokenType = 'word')))
      testWords <- unlist(NLPStudio::tokenize(x = test$content, tokenType = 'word'))
      oov <- testWords[!testWords %fin% vocabulary]
      test$content <-
        stringi::stri_replace_all_regex(str = test$content,
                                        pattern = paste0('\\b', oov, '\\b'),
                                        replacement = 'UNK',
                                        vectorize_all = FALSE)

      document$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..test$setMeta(key = 'OOV', value = length(oov), type = 'q')
      private$..test$addDocument(document)
      return(TRUE)
    },

    annotate = function(corpus) {
      # Assumes a single document Corpus
      document <- corpus$getDocuments()[[1]]
      document$content <-
        paste(paste0(rep("<s>", times = private$..modelSize-1), collapse = " "),
              document$content,
              "</s>", sep = " ")
      corpus$addDocument(document)
      return(corpus)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(train, test, modelSize = 3, openVocabulary = TRUE) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('train', 'test')
      private$..params$classes$objects <- list(train, test)
      private$..params$classes$valid <- list('Corpus','Corpus')
      private$..params$logicals$variables <- c('openVocabulary')
      private$..params$logicals$values <- c(openVocabulary)
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      # Dock language model (extract members read/modified in class)
      private$..train <- Clone$new()$this(x = train)
      private$..test <- Clone$new()$this(x = test)
      private$..modelSize <- modelSize
      private$..openVocabulary <- openVocabulary

      invisible(self)
    },

    prepTrain = function() {

      private$..train <- private$combine(private$..train)

      if (private$..openVocabulary) private$processTrainOOV()

      private$..train <- private$annotate(private$..train)

      invisible(self)
    },

    prepTest = function() {

      private$..test <- private$combine(private$..test)

      if (private$..openVocabulary) private$processTestOOV()

      private$..test <- private$annotate(private$..test)

      invisible(self)
    },

    getTrain = function() { return(private$..train) },
    getTest = function() { return(private$..test) },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$lmCorpora(self)
    }
  )
)
