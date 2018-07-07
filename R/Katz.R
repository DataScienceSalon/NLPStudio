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
    #                                    qBO                                  #
    #  Computes back-off probabilities for observed and unobserved nGrams     #
    #-------------------------------------------------------------------------#
    qBO = function() {

      for (i in 2:private$..settings$modelSize) {

        private$..model$nGrams[[i]]$qBOA <- private$..model$nGrams[[i]]$cKatz /
          private$..model$nGrams[[i]]$cPrefix

        private$..model$nGrams[[i]]$qBOTail <- private$..model$nGrams[[i]]$cSuffix /
          sum(private$..model$nGrams[[i-1]]$cNGram)

        private$..model$nGrams[[i]]$sumqBOBTail <-
          (sum(private$..model$nGrams[[i-1]]$cNGram) -
          private$..model$nGrams[[i]]$cSuffix) /
          sum(private$..model$nGrams[[i-1]]$cNGram)

        private$..model$nGrams[[i]]$gBOB <-
          private$..model$nGrams[[i]]$alpha *
          private$..model$nGrams[[i]]$qBOTail /
          private$..model$nGrams[[i]]$sumqBOBTail
      }

    },

    #-------------------------------------------------------------------------#
    #                              Compute Alpha                              #
    #-------------------------------------------------------------------------#
    alpha = function() {

      for (i in 2:(private$..settings$modelSize)) {
        cKatzSum <- private$..model$nGrams[[i]] %>% group_by(prefix) %>%
          summarise(cKatzSum = sum(cKatz))
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                       cKatzSum, by.x = 'prefix',
                       by.y = 'prefix')
        private$..model$nGrams[[i]]$alpha <-
          1 - (private$..model$nGrams[[i]]$cKatzSum /
                 private$..model$nGrams[[i]]$cPrefix)
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                               cPrefix                                   #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    cFix = function() {

      for (i in 2:private$..settings$modelSize) {
        lower <- private$..model$nGrams[[i-1]] %>% select(nGram, cNGram)
        names(lower) <- c("nGram", "cPrefix")
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                                             lower, by.x = "prefix",
                                             by.y = "nGram", all = TRUE)

        names(lower) <- c("nGram", "cSuffix")
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                                             lower, by.x = "suffix",
                                             by.y = "nGram")
      }
    },

    #-------------------------------------------------------------------------#
    #                                cKatz                                    #
    #                 Discounted Counts for Observed Unigrams                 #
    #-------------------------------------------------------------------------#
    cKatz = function() {

      for (i in 2:private$..settings$modelSize) {
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

      # Initialize ngram tables with raw frequency counts
      private$initTables()

      # Compute unigram maximum likelihood probability
      private$pML()

      # Add discounted counts
      private$cKatz()

      # Add discounted probability mass from observed nGrams
      private$pKatz()

      # Compute alpha, the discounted probability mass
      private$alpha()

      names(private$..model$nGrams) <- modelTypes[1:modelSize]


      invisible(self)
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
