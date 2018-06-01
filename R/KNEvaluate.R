#==============================================================================#
#                               KNEvaluate                                     #
#==============================================================================#
#' KNEvaluate
#'
#' \code{KNEvaluate} Evaluates a Kneser-Ney Language Model on Test Set
#'
#' Evaluates a Kneser-Ney Language Model on Test Set by computing
#' test set perplexity
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family LMStudio Classes
#' @export
KNEvaluate <- R6::R6Class(
  classname = "KNEvaluate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(

    ..model = character(),
    ..test = character(),
    ..nGrams = list(),
    ..modelSize = numeric(),
    ..scores = data.table(),
    ..nWords = 0,
    ..nSentence = 0,
    ..nSentences = numeric(),
    ..nGram = numeric(),

    scoreNGram = function(ngram, nGramOrder) {
      prob <- private$..nGrams[[nGramOrder]][ nGram == ngram][, pKN]
      if (length(prob) == 0) {
        ngram <- gsub(pattern = "^([\\w\\-]+) ", replacement = "",x = ngram, perl = TRUE)
        nGramOrder <- nGramOrder - 1
        prob <- private$scoreNGram(ngram, nGramOrder)
      } else {
        res <- list()
        res$prob <- prob
        res$nGramOrder <- nGramOrder
        return(res)
      }
    },

    scoreSentence = function(sentence) {
      nGrams <- unlist(NLPStudio::tokenize(sentence, tokenType = 'word',
                                           nGrams = private$..modelSize))
      nGramScores <- lapply(nGrams, function(n) {
        private$..nGram <- private$..nGram + 1
        s <- private$scoreNGram(n, private$..modelSize)
        score <- list()
        score$nSentence <- private$..nSentence
        score$nGramNum <- private$..nGram
        score$nGram <- n
        score$match <- s$nGramOrder
        score$pKN <- s$prob
        score
      })
    },

    scoreTestData = function() {
      document <- private$..test$getDocuments()[[1]]
      scores <- rbindlist(lapply(document$content, function(d) {
        private$..nSentence <- private$..nSentence + 1
        private$..nGram <- 0
        private$scoreSentence(d)
      }))
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {


      private$loadDependencies()

      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('KNModel'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..model <- x
      private$..test <- x$getTest()
      private$..nGrams <- x$getNGrams()
      private$..modelSize <- x$getModelSize()

      invisible(self)

      event <-  paste0("Instantiated KNEvaluate ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    evaluate = function() {

      private$scoreTestData()
      private$perplexity()


      return(private$..model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knEvaluate(self)
    }
  )
)
