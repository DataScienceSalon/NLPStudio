#==============================================================================#
#                               SLMEvaluate                                    #
#==============================================================================#
#' SLMEvaluate
#'
#' \code{SLMEvaluate} Evaluates a designated language model on a test set.
#'
#' @param config SLMConfig object
#' @param model A Language Model Object
#' @param test A test Corpus object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family SLMStudio Classes
#' @export
SLMEvaluate <- R6::R6Class(
  classname = "SLMEvaluate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLMStudio0,

  private = list(

    ..test = character(),
    ..nGrams = list(),
    ..modelSize = numeric(),
    ..modelTypes = character(),
    ..scores = data.table(),
    ..evaluation = data.frame(),

    evalSummary = function() {

      NLPStudio::printHeading(text = 'Evaluation', symbol = "-", newlines = 2)
      print(private$..evaluation)
      return(TRUE)

    },

    score = function() {

      # Obtain training test summary data.

      # Obtain networds and subtract zero probability words
      netWords <- private$..test$getMeta(key = 'netWords')
      OOV = private$..test$getMeta(key = 'OOV')
      exact <- nrow(private$..scores[ Match == private$..modelSize])
      exactRate <- exact / nrow(private$..scores)
      zeroProbs <- nrow(private$..scores[ pKN == 0])
      scores <- private$..scores[ pKN > 0]
      scores[, logProb := log2(pKN)]
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
      nGrams <- unlist(NLPStudio::tokenize(document$content, tokenUnit = 'word',
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
    }

  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(config, model, test) {

      private$loadServices()

      private$..params <- list()
      private$..params$classes$name <- list('config', 'model', 'test')
      private$..params$classes$objects <- list(config, model, test)
      private$..params$classes$valid <- list('SLMConfig', 'KN', 'Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..model <- model
      private$..config <- config
      private$..test <- test
      private$..nGrams <- model$getNGrams()
      private$..modelSize <- config$getModelSize()
      private$..modelTypes <- config$getModelTypes()

      invisible(self)

      event <-  paste0("Instantiated SLMEvaluate ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    evaluate = function() {

      private$buildTestNGrams()
      private$computeProbs()
      private$score()
      private$..model$setScores(private$..scores)
      private$..model$setEval(private$..evaluation)

      invisible(self)
    },

    summary = function() {
      private$evalSummary()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$slmEvaluate(self)
    }
  )
)
