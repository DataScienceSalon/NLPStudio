#==============================================================================#
#                                   Katz                                       #
#==============================================================================#
#' Katz
#'
#' \code{Katz} Kneser Ney Statistical Learning Model
#'
#' Encapsulates a Statistical Language Model implementing the Kneser-Ney
#' algorithm.
#'
#' @param x a CVSet containing training and test Corpus objects
#' @param train Train Corpus object. Ignored if x is a CVSet, required otherwise.
#' @param test Test Corpus object. Ignored if x is a CVSet, required otherwise.
#' @param modelSize Numeric between 1, for unigram to 5 for quintgram.
#' @param openVocabulary Logical. If TRUE, preprocessing will replace all words
#' in the test corpus that are not in the training corpus with the pseudo-word
#' UNK. If FALSE, all words in test corpus are assumed to be in the training
#' corpus. The default is TRUE.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @export
Katz <- R6::R6Class(
  classname = "Katz",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  private = list(

    #-------------------------------------------------------------------------#
    #                                 qBOA                                    #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    qBOA = function(i) {

      if (i == 1) {
        private$..model$nGrams[[i]]$pML <- private$..model$nGrams[[i]]$cNGram /
          sum(private$..model$nGrams[[i]]$cNGram)
      } else {
        private$..model$nGrams[[i]]$qBO <- private$..model$nGrams[[i]]$cKatz /
          private$..model$nGrams[[i]]$cPrefix
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                               cPrefix                                   #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    cPrefix = function(i) {

      if (i > 1) {
        lower <- private$..model$nGrams[[i-1]] %>% select(nGram, cNGram)
        names(lower) <- c("nGram", "cPrefix")
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                                             lower, by.x = "prefix",
                                             by.y = "nGram", all.x = TRUE)
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                cKatz                                    #
    #                 Discounted Counts for Observed Unigrams                 #
    #-------------------------------------------------------------------------#
    cKatz = function(i) {

      if (private$..settings$gtDiscount) {
        discounts <- as.numeric(private$..model$discounts[i])
        private$..model$nGrams[[i]]$cKatz <-
          ifelse (private$..model$nGrams[[i]]$cNGram > 5,
                  private$..model$nGrams[[i]]$cNGram,
                  private$..model$nGrams[[i]]$cNGram *
                    discounts[private$..model$nGrams[[i]]$cNGram+1])
      } else {
        private$..model$nGrams[[i]]$cKatz <-
          ifelse (private$..model$nGrams[[i]]$cNGram > 5,
                  private$..model$nGrams[[i]]$cNGram,
                  private$..model$nGrams[[i]]$cNGram -
                    private$..settings$fixedDiscount)
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                              discounts                                  #
    #         Computes discount rates to be applied to raw nGram counts       #
    #-------------------------------------------------------------------------#
    discounts = function() {

      k <- private$..settings$k
      n <- private$..settings$modelSize

      private$..model$discounts <- rbindlist(lapply(seq(1:n), function(i) {

        nTotal <- as.numeric(private$..model$totals %>% filter(n == i) %>%
                               select(nGramTypes))
        d_r <- list()
        for (r in 1:k) {

          n_1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 1))
          if (private$..settings$estimateGT) {
            # Use expected value of counts instead of counts
            rStar <-  (r / (r+1)) * (1 - (n_1 / nTotal))
          } else {
            # Use actual counts
            n_rPLus1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == (r + 1)))
            n_r <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == r))
            rStar <- (r + 1) * n_rPLus1 / n_r
          }

          # Compute Discount Rate
          n_kPLus1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == (k+1)))
          k_ratio <- (k+1) * n_kPLus1 / n_1
          d_r[[r]] <- ((rStar / r) - k_ratio) / (1 - k_ratio)
        }
        df <- cbind(as.data.frame(d_r))
        df <- cbind(i, df)
        names(df) <- c('nGram',seq(1:k))
        df
      }))
      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                                build                                    #
    #         Driver method for computing nad building the nGram tables.      #
    #-------------------------------------------------------------------------#
    build = function() {

      private$initTrainTables()
      private$discounts()

      for (i in 1:private$..settings$modelSize) {
        private$cKatz(i)
        private$cPrefix(i)
        private$qBOA(i)
      }
    },
    #-------------------------------------------------------------------------#
    #                             EVALUATE METHODS                            #
    #-------------------------------------------------------------------------#
    #-------------------------------------------------------------------------#
    #                              Compute Alpha                              #
    #-------------------------------------------------------------------------#
    alpha = function(ngram, n) {

      pfx <- gsub(private$..regex$prefix[[n-1]], "\\1", ngram, perl = TRUE)

      alpha <- private$..model$nGrams[[n]] %>% filter(prefix == pfx) %>%
        summarise(cKatz = sum(cKatz), cPrefix = unique(cPrefix))

      alpha <- 1 - (alpha$cKatz / alpha$cPrefix)

      return(alpha)
    },
    #-------------------------------------------------------------------------#
    #                               qBOB                                      #
    #            Computes sum of probabilities from unobserved nGrams         #
    #-------------------------------------------------------------------------#
    qBOB = function(pfx, sfx, n) {

      # Divide nGrams starting with pfx into observed (A) and unobserved (B)
      A <- (private$..model$nGrams[[n+1]] %>%
               filter(prefix == pfx) %>% select(tail))$tail
      B <- private$..corpora$vocabulary[!private$..corpora$vocabulary %in% A]

      if (n == 1) {
        unobservedNGrams <- B
      } else {
        # Format nGrams that complete unobserved n + 1 Grams
        newPrefix <- gsub(private$..regex$suffix[[n-1]], "\\1", pfx, perl = TRUE)
        unobservedNGrams <- unlist(lapply(B, function(b) {
              paste(paste(newPrefix, b), collapse = " ")
        }))
      }

      # Compute sums back of probabilities
      qBOBSum <- sum(unlist(lapply(unobservedNGrams, function(u) {
        private$qBO(u, n)
      })))

      return(qBOBSum)
    },

    #-------------------------------------------------------------------------#
    #                           scoreNGram                                    #
    #               Assigns a probability to each test nGram                  #
    #-------------------------------------------------------------------------#
    qBO = function(ngram, n) {

      if (n == 1) {
        qBO <- as.numeric(private$..model$nGrams[[n]] %>% filter(nGram == ngram) %>%
          select(pML))
      } else {
        qBO <- as.numeric(private$..model$nGrams[[n]] %>% filter(nGram == ngram) %>%
          select(qBO))
      }

      if (is.na(qBO)) {
        # Split nGram into prefix and suffix
        pfx <- gsub(private$..regex$prefix[[n-1]], "\\1", ngram, perl = TRUE)
        sfx <- gsub(private$..regex$suffix[[n-1]], "\\1", ngram, perl = TRUE)

        # Compute alpha
        alpha <- private$alpha(ngram, n)

        # Compute lambda numerator: the backoff probability of nGram tail
        qBOSfx <- private$qBO(sfx, n-1)

        # Compute the probability mass for the n-1 ngrams that complete
        # unobserved  nGrams that share the same prefix
        qBOBSum <- private$qBOB(pfx, sfx, n-1)

        # Compute qBO
        qBO <- alpha * qBOSfx / qBOBSum
      }
      return(qBO)
    },

    #-------------------------------------------------------------------------#
    #                           scoreNGram                                    #
    #               Assigns a probability to each test nGram                  #
    #-------------------------------------------------------------------------#
    scoreNGram = function(ngram, n) {

      # If exact match return discounted probability
      score <- as.numeric(private$..model$nGrams[[n]] %>%
        filter(nGram == ngram) %>% select(qBO))

      # Otherwise estimate score from unobserved nGrams
      if (is.na(score)) {
        score <- private$qBO(ngram, n)
      }
      return(score)
    },

    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Prepare perplexity scores for test set                    #
    #-------------------------------------------------------------------------#
    score = function() {

      private$initTestTable()

      private$..evaluation$scores %>% private$..evaluation$scores %>%
        mutate(score = scoreNGram(nGram, n = private$..settings$modelSize),
               logScore = log(score),
               exact = checkMatch(nGram))

      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    validateParams = function(train = NULL, name = NULL, modelSize, k,
                              gtDiscount, estimateGT, fixedDiscount,
                              openVocabulary, bos) {

      private$..params <- list()
      private$..params$classes$name <- list('train')
      private$..params$classes$objects <- list(train)
      private$..params$classes$valid <- list('Corpus')
      private$..params$range$variable <- c('modelSize', 'fixedDiscount')
      private$..params$range$value <- c(modelSize, fixedDiscount)
      private$..params$range$low <- c(1,0.1)
      private$..params$range$high <- c(5, 0.95)
      private$..params$logicals$variables <- c('openVocabulary', "bos")
      private$..params$logicals$values <- c(openVocabulary, bos)
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
    initialize = function(train = NULL, name = NULL, modelSize = 3, k = 5,
                          gtDiscount = TRUE, estimateGT = FALSE, bos = TRUE,
                          fixedDiscount = 0.5, openVocabulary = TRUE) {

      private$loadServices(name)

      private$validateParams(train, name, modelSize, k, gtDiscount,
                             estimateGT, fixedDiscount, openVocabulary,
                             bos)

      # Update settings
      private$..settings$modelName <- name
      private$..settings$modelSize <- modelSize
      private$..settings$k <- k
      private$..settings$gtDiscount <- gtDiscount
      private$..settings$estimateGT <- estimateGT
      private$..settings$fixedDiscount <- fixedDiscount
      private$..settings$algorithm <- 'Kneser-Ney'
      private$..settings$modelType <- private$..settings$modelTypes[modelSize]
      private$..settings$openVocabulary <- openVocabulary
      private$..settings$bos <- bos

      # Update meta data
      private$meta$set(key = 'algorithm', value = 'Kneser-Ney', type = 'f')
      private$meta$set(key = 'openVocabulary', value = openVocabulary, type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType',
                       value = private$..settings$modelTypes[modelSize],
                       type = 'f')
      private$meta$set(key = 'k', value = k, type = 'f')
      private$meta$set(key = 'estimateGT', value = estimateGT, type = 'f')
      private$meta$set(key = 'gtDiscount', value = gtDiscount, type = 'f')
      private$meta$set(key = 'fixedDiscount', value = fixedDiscount, type = 'f')


      private$..corpora$train <- train

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                Fit Method                               #
    #-------------------------------------------------------------------------#
    fit = function() {

      # Prepare training corpus
      private$prepTrain()

      # Build tables
      private$build()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Evaluate Method                              #
    #-------------------------------------------------------------------------#
    evaluate = function(test) {

      # Validate
      private$..params <- list()
      private$..params$classes$name <- list('test')
      private$..params$classes$objects <- list(test)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'evaluate', event = v$msg, level = "Error")
        stop()
      }

      private$..corpora$test <- test

      # Prepare test corpus
      private$prepTest()

      # Score test set
      private$score()

      invisble(self)
    },

    #-------------------------------------------------------------------------#
    #                            getEval Method                               #
    #-------------------------------------------------------------------------#
    getEval = function() {

    },

    #-------------------------------------------------------------------------#
    #                            Predict Method                               #
    #-------------------------------------------------------------------------#
    predict = function(test = NULL) {

      # Validate and store test set.
      private$validateClass(x = test, fieldName = 'testSet',
                            className = 'Katz' , methodName = 'predict')
      private$..corpora$test <- test

      # Preprocess test set
      private$prepTest()

      # Estimate test set
      private$estimate()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$katz(self)
    }
  )
)
