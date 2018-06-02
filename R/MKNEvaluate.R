#==============================================================================#
#                               MKNEvaluate                                     #
#==============================================================================#
#' MKNEvaluate
#'
#' \code{MKNEvaluate} Evaluates a Kneser-Ney Language Model on Test Set
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
MKNEvaluate <- R6::R6Class(
  classname = "MKNEvaluate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNEvaluate,

  private = list(

    score = function() {

      # Obtain training test summary data.

      # Obtain networds and subtract zero probability words
      netWords <- private$..test$getMeta(key = 'netWords')
      OOV = private$..test$getMeta(key = 'OOV')
      exact <- nrow(private$..scores[ Match == private$..modelSize])
      exactRate <- exact / nrow(private$..scores)
      zeroProbs <- nrow(private$..scores[ pMKN == 0])
      scores <- private$..scores[ pMKN > 0]
      scores[, logProb := log2(pMKN)]
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
        probs <- private$..nGrams[[i]][,.(nGram, pMKN, Match = i)]

        if (i == 1) {
          setkey(private$..scores, Unigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Unigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pMKN.update) & (Match == 0)),
                           c("Match", "pMKN") := list(Match.update, pMKN.update)]
          private$..scores[, c('pMKN.update', 'Match.update') := NULL]
        } else if (i == 2) {
          setkey(private$..scores, Bigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Bigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pMKN.update) & (Match == 0)),
                           c("Match", "pMKN") := list(Match.update, pMKN.update)]
          private$..scores[, c('pMKN.update', 'Match.update') := NULL]

        } else if (i == 3) {
          setkey(private$..scores, Trigram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Trigram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pMKN.update) & (Match == 0)),
                           c("Match", "pMKN") := list(Match.update, pMKN.update)]
          private$..scores[, c('pMKN.update', 'Match.update') := NULL]

        } else if (i == 4) {
          setkey(private$..scores, Quadgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quadgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pMKN.update) & (Match == 0)),
                           c("Match", "pMKN") := list(Match.update, pMKN.update)]
          private$..scores[, c('pMKN.update', 'Match.update') := NULL]
        } else if (i == 5) {
          setkey(private$..scores, Quintgram)
          private$..scores <- merge(private$..scores, probs, by.x = 'Quintgram',
                                    by.y = 'nGram', all.x = TRUE,
                                    suffixes = c("", ".update"))
          private$..scores[(!is.na(pMKN.update) & (Match == 0)),
                           c("Match", "pMKN") := list(Match.update, pMKN.update)]
          private$..scores[, c('pMKN.update', 'Match.update') := NULL]
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
      private$..scores <- cbind(private$..scores, Match = 0, pMKN = 0)
      return(TRUE)
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
      private$..params$classes$valid <- list(c('MKN'))
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

      event <-  paste0("Instantiated MKNEvaluate ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    build = function() {

      private$buildTestNGrams()
      private$computeProbs()
      private$score()
      private$..model$setScores(private$..scores)
      private$..model$setEval(private$..evaluation)

      # Remove temporary members
      private$..test <- NULL
      private$..nGrams <- NULL
      private$..modelSize <- NULL
      private$..modelTypes <- NULL
      private$..scores <- NULL
      private$..evaluation <- NULL

      return(private$..model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknEvaluate(self)
    }
  )
)
