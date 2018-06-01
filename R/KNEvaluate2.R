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
    ..modelTypes = character(),
    ..scores = data.table(),
    ..nWords = 0,
    ..nSentence = 0,
    ..nSentences = numeric(),
    ..nGram = numeric(),

    scoreNGram = function(ngram, nGramOrder) {
      print(ngram)
      prob <- private$..nGrams[[nGramOrder]][ nGram == ngram][, pKN]
      if (length(prob) == 0) {
        ngram <- gsub(pattern = "^([\\w'\\-]+) ", replacement = "",x = ngram, perl = TRUE)
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
      nGrams <- unlist(NLPStudio::tokenize(text, tokenType = 'word',
                                           nGrams = private$..modelSize,
                                           lowercase = FALSE))
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

    computeScores = function() {

      i <- private$..modelSize

      while (i > 0) {
        # Obtain training probabilities
        setkey(private$..nGrams[[i]], nGram)
        probs <- private$..nGrams[[i]][,.(nGram, pKN, Match = i)]

        if (i == 1) {
          setkey(private$..scores, Unigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Unigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pKN.update) & (Match == 0)),
                           c("Match", "pKN") := list(Match.update, pKN.update)]
          private$..scores[, c('pKN.update', 'Match.update') := NULL]
        } else if (i == 2) {
          setkey(private$..scores, Bigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Bigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pKN.update) & (Match == 0)),
                           c("Match", "pKN") := list(Match.update, pKN.update)]
          private$..scores[, c('pKN.update', 'Match.update') := NULL]

        } else if (i == 3) {
          setkey(private$..scores, Trigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Trigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pKN.update) & (Match == 0)),
                           c("Match", "pKN") := list(Match.update, pKN.update)]
          private$..scores[, c('pKN.update', 'Match.update') := NULL]

        } else if (i == 4) {
          setkey(private$..scores, Quadgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quadgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pKN.update) & (Match == 0)),
                           c("Match", "pKN") := list(Match.update, pKN.update)]
          private$..scores[, c('pKN.update', 'Match.update') := NULL]
        } else if (i == 5) {
          setkey(private$..scores, Quintgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quintgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pKN.update) & (Match == 0)),
                           c("Match", "pKN") := list(Match.update, pKN.update)]
          private$..scores[, c('pKN.update', 'Match.update') := NULL]
        }
        i <- i - 1
      }

    },


    buildTestNGrams = function() {
      document <- private$..test$getDocuments()[[1]]
      nGrams <- unlist(NLPStudio::tokenize(document$content, tokenType = 'word',
                                           nGrams = private$..modelSize,
                                           lowercase = FALSE))
      nBackOff <- 1
      private$..scores <- data.table(nGrams = nGrams)
      while (nBackOff < private$..modelSize) {
        nGrams <- gsub(pattern = "^([\\w'\\-]+) ",
                       replacement = "",x = nGrams, perl = TRUE)
        private$..scores <- cbind(private$..scores, nGrams)
        nBackOff <- nBackOff + 1
      }
      modelTypes <- private$..modelTypes[1:private$..modelSize]
      modelTypes <- rev(modelTypes)
      names(private$..scores) <- modelTypes[seq(1:private$..modelSize)]
      private$..scores <- cbind(private$..scores, Match = 0, pKN = 0)
      return(TRUE)
    },

    scoreTestData = function() {

      private$buildTestNGrams()
      private$computeScores()
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
      private$..modelTypes <- x$getModelTypes()

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
