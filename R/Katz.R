#==============================================================================#
#                                   Katz                                       #
#==============================================================================#
#' Katz
#'
#' \code{Katz} Katz Statistical Learning Model
#'
#' Encapsulates a Statistical Language Model implementing the Katz backoff
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

    #=========================================================================#
    #                        COMMON BUILD METHODS                             #
    #=========================================================================#

    #-------------------------------------------------------------------------#
    #                            computeQML                                   #
    #      Computes maximum likelihood probabilities for unigrams             #
    #-------------------------------------------------------------------------#
    computeQML = function(i, nGrams) {

      if (i == 1) {

        nGrams$qML <- nGrams$cNGram / sum(private$..model$nGrams[[i]]$cNGram)

        # Replace NA values with zeros
        nGrams[is.na(nGrams)] <- 0

      }
      return(nGrams)
    },
    #-------------------------------------------------------------------------#
    #                           computeCKatz                                  #
    # Compute discounted counts for observed nGrams in the language model     #
    #-------------------------------------------------------------------------#
    computeCKatz = function(i, nGrams) {

      if (i > 1) {

        # Apply Good-Turing discount unless the fixedDiscount parameter is TRUE
        if (private$..parameters$gtDiscount) {
          discounts <- as.numeric(private$..model$discounts[i])
          nGrams$cKatz <-
            ifelse (nGrams$cNGram > 5,
                    nGrams$cNGram,
                    nGrams$cNGram *
                      discounts[cNGram])

        } else {
          nGrams$cKatz <-
            ifelse (nGrams$cNGram > 5,
                    nGrams$cNGram,
                    nGrams$cNGram -
                      private$..parameters$fixedDiscount)
        }

        # Set negative counts to zero
        nGrams$cKatz <- ifelse(nGrams$cKatz < 0, 0, nGrams$cKatz)

        # Replace NA values with zeros
        nGrams[is.na(nGrams)] <- 0
      }
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                          computeCPrefix                                 #
    #                Compute counts for each nGram prefix                     #
    #-------------------------------------------------------------------------#
    computeCPrefix = function(i, nGrams) {

      if (i > 1) {
        # Obtain lower order nGram counts and merge with higher level by prefix
        lower <- private$..model$nGrams[[i-1]][,c("nGram", "cNGram")]
        names(lower) <- c("prefix", "cPrefix")
        nGrams <- merge(nGrams, lower, by = "prefix", all.x = TRUE)

        # Count start of sentence tokens
        nGrams$cPrefix <- ifelse(nGrams$prefix == paste(rep("BOS", (i-1)), collapse = " "),
                                 private$..corporaStats$train$sentences,
                                 nGrams$cPrefix)

        # Replace NA values with zeros
        nGrams[is.na(nGrams)] <- 0
      }
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                         computeQBOA                                     #
    #           Computes probability of observed nGrams                       #
    #-------------------------------------------------------------------------#
    computeQBOA = function(i, nGrams) {

      if (i > 1) {

        nGrams$qBOA <- nGrams$cKatz / nGrams$cPrefix

        # Replace NA values with zeros
        nGrams[is.na(nGrams)] <- 0
      }

      return(nGrams)
    },


    #=========================================================================#
    #                           EVALUATION METHODS                            #
    #=========================================================================#

    #-------------------------------------------------------------------------#
    #                           computeAlpha                                  #
    #       Compute alpha, the probability mass taken by discounting          #
    #-------------------------------------------------------------------------#
    computeAlpha = function(i) {

      if (i > 1) {

        cKatzPfxSum <- private$..model$nGrams[[i]] %>% group_by(prefix) %>%
          summarise(cKatzPfxSum = sum(cKatz)) %>% select(prefix, cKatzPfxSum)


        # Add the values to the evaluation nGram table
        private$..eval$nGrams[[i]] <- merge(private$..eval$nGrams[[i]],
                                            cKatzPfxSum, by = "prefix", all.x = TRUE)

        # Replace NA values with zeros
        private$..eval$nGrams[[i]][is.na(private$..eval$nGrams[[i]])] <- 0

        # Compute alpha
        private$..eval$nGrams[[i]] <-
          private$..eval$nGrams[[i]] %>%
          mutate(alpha = 1 - (cKatzPfxSum / cPrefix))

        # Replace NA with 1, since NA implies zero counts
        private$..eval$nGrams[[i]]$alpha[is.na(private$..eval$nGrams[[i]]$alpha)] <- 1

      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             computeLambda                               #
    # Compute lambda, the redistribution of discounted probability mass over  #
    # unobserved nGrams                                                       #
    #-------------------------------------------------------------------------#
    computeLambda = function(i) {

      if (i > 1) {

        # Add suffix probabilities to evaluation nGram table
        if (i == 2) {
          qBO = private$..eval$nGrams[[i-1]] %>% select(nGram, qML)
        } else {
          qBO = private$..eval$nGrams[[i-1]] %>% select(nGram, qBO)
        }
        names(qBO) <- c("suffix", "qSfx")

        # Compute numerator
        private$..eval$nGrams[[i]] <- merge(private$..eval$nGrams[[i]],
                                            qBO, by = 'suffix', all.x = TRUE)

        # Compute Denominator
        # Get lower order observeed nGram probabilities
        if (i == 2) {
          qBO = private$..model$nGrams[[i-1]] %>% select(nGram, qML)
        } else {
          qBO = private$..model$nGrams[[i-1]] %>% select(nGram, qBOA)
        }
        names(qBO) <- c("suffix", "qSfx")

        # Get observed prefix suffix combinations
        pfxSfx <- private$..model$nGrams[[i]] %>% select(prefix, suffix)

        # Merge in suffix probabilities
        qSfxSum <- merge(pfxSfx, qBO, by = 'suffix', all.x = TRUE)

        # Summarize probabilities by prefix
        qSfxSum <- qSfxSum %>% group_by(prefix) %>% summarise(qSfxSum = sum(qSfx)) %>%
          select(prefix, qSfxSum)

        # Add the above to eval nGrams by prefix
        private$..eval$nGrams[[i]] <- merge(private$..eval$nGrams[[i]], qSfxSum,
                                            by = 'prefix', all.x = TRUE)
        private$..eval$nGrams[[i]][is.na(private$..eval$nGrams[[i]])] <- 0

        # Compute beta as 1 - the previous sum
        private$..eval$nGrams[[i]]$beta <- 1 - private$..eval$nGrams[[i]]$qSfxSum

        # Compute lambda
        private$..eval$nGrams[[i]]$lambda <-
          private$..eval$nGrams[[i]]$qSfx /
          private$..eval$nGrams[[i]]$beta

      }

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                         computeQBOB                                     #
    #         Compute nGram probability based upon unobserved nGrams          #
    #-------------------------------------------------------------------------#
    computeQBOB = function(i) {

      if (i > 1) {

        private$..eval$nGrams[[i]]$qBOB <-
          private$..eval$nGrams[[i]]$alpha *
          private$..eval$nGrams[[i]]$lambda

        # Replace NA values with zeros
        private$..eval$nGrams[[i]][is.na(private$..eval$nGrams[[i]])] <- 0

      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             computeQBO                                  #
    #                  Compute final backoff probability                      #
    #-------------------------------------------------------------------------#
    computeQBO = function(i) {

      if (i > 1) {
        private$..eval$nGrams[[i]]$qBO <-
          ifelse(private$..eval$nGrams[[i]]$qBOA == 0,
                 private$..eval$nGrams[[i]]$qBOB,
                 private$..eval$nGrams[[i]]$qBOA)

        # Replace NA values with zeros
        private$..eval$nGrams[[i]][is.na(private$..eval$nGrams[[i]])] <- 0
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                            buildEval                                    #
    #      Builds evaluation tables which store counts for the test data      #
    #-------------------------------------------------------------------------#
    buildEval = function(){

      private$prepTest()
      private$initEvalNGrams()

      for (i in 1:private$..parameters$modelSize) {

        # Compute back off probabilities for observed nGrams
        private$..eval$nGrams[[i]] <- private$computeQML(i, private$..eval$nGrams[[i]])
        private$..eval$nGrams[[i]] <- private$computeCKatz(i, private$..eval$nGrams[[i]])
        private$..eval$nGrams[[i]] <- private$computeCPrefix(i, private$..eval$nGrams[[i]])
        private$..eval$nGrams[[i]] <- private$computeQBOA(i, private$..eval$nGrams[[i]])

        # Compute backoff probabilities for unobserved nGrams
        private$computeAlpha(i)
        private$computeLambda(i)
        private$computeQBOB(i)
        private$computeQBO(i)
      }
      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Computes perplexity score for test data                   #
    #-------------------------------------------------------------------------#
    score = function(){

      # Extract backoff probabilities at highest nGram level
      private$..eval$scores <-
        private$..eval$nGrams[[private$..parameters$modelSize]] %>%
        select(nGram, qBO)
      names(private$..eval$scores) <- c("nGram", "p")

      # Clean up NA and inf values
      private$..eval$scores[is.infinite(p) | is.na(p), p := 0]

      # Count number of NGrams
      private$..eval$score$nGrams <-
        nrow(private$..eval$scores) - 1

      # Compute oov rate
      private$..eval$score$oovRate <-
        private$..eval$score$oov /
        private$..eval$score$nGrams

      # Count number of zero probabilities
      private$..eval$score$zeroProbs <-
        nrow(private$..eval$scores %>% filter(p == 0))

      # Compute Zero Prob Rate
      private$..eval$score$zeroProbRate <-
        private$..eval$score$zeroProbs /
        private$..eval$score$nGrams

      # Compute Log Probability from non-zero probabilities
      nonZero <- private$..eval$scores %>% filter(p > 0)
      private$..eval$score$logProb <-
        sum(log(nonZero$p))

      # Compute perplexity
      private$..eval$score$perplexity <-
        2^(-private$..eval$score$logProb /
             private$..corporaStats$test$N)

      return(TRUE)
    },

    #=========================================================================#
    #                          MODEL BUILDING METHODS                         #
    #=========================================================================#

    #-------------------------------------------------------------------------#
    #                              discounts                                  #
    #         Computes discount rates to be applied to raw nGram counts       #
    #-------------------------------------------------------------------------#
    computeDiscounts = function() {

      k <- private$..parameters$k
      n <- private$..parameters$modelSize

      private$..model$discounts <- rbindlist(lapply(seq(1:n), function(i) {

        nTotal <- as.numeric(private$..model$totals %>% filter(n == i) %>%
                               select(nGramTypes))
        d_r <- list()
        for (r in 1:k) {

          # Compute adjusted count
          n_1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 1))
          n_rPLus1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == (r + 1)))
          n_r <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == r))

          # If n_r = 0, use expected counts, otherwise us actual counts
          if (n_r == 0) {
            rStar <-  (r / (r+1)) * (1 - (n_1 / nTotal))
          } else {
            rStar <- (r + 1) * n_rPLus1 / n_r
          }

          # Compute Discount Rate
          n_kPLus1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == (k+1)))
          k_ratio <- (k+1) * n_kPLus1 / n_1
          d_r[[r]] <- ((rStar / r) - k_ratio) / (1 - k_ratio)
          d_r[[r]][is.na(d_r[[r]])] <- 0
        }
        df <- cbind(as.data.frame(d_r))
        df <- cbind(i, df)
        names(df) <- c('nGram',seq(1:k))
        df
      }))
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                               buildModel                                #
    # Build language model tables, including nGrams, totals and discounts.    #
    #-------------------------------------------------------------------------#
    buildModel = function() {

      # Annotate text, initialize model tables and compute totals and discounts
      private$prepTrain()
      private$initModel()
      private$computeTotals()
      private$computeDiscounts()

      for (i in 1:private$..parameters$modelSize) {
        private$..model$nGrams[[i]] <- private$computeQML(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$computeCKatz(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$computeCPrefix(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$computeQBOA(i, private$..model$nGrams[[i]])
      }

      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                         printOverview Method                            #
    #                 Summarizes the model and parameters                     #
    #-------------------------------------------------------------------------#
    printOverview = function() {

      meta <- private$meta$get()

      heading <- paste(private$..parameters$modelName, "Summary")
      discountType <- ifelse(private$..parameters$gtDiscount, "Good Turing",
                             "Fixed")

      NLPStudio::printHeading(text = heading, symbol = "=", newlines = 2)

      cat(paste0("\nId            : ", meta$identity$id))
      cat(paste0("\nName          : ", private$..parameters$modelName))
      cat(paste0("\nType          : ", private$..parameters$modelType))
      cat(paste0("\nDiscount Type : ", discountType))
      if (private$..parameters$gtDiscount == FALSE) {
        cat(paste0("\nDiscount      : ", private$..parameters$fixedDiscount))
      }
      cat(paste0("\nVocabulary    : ", private$..parameters$vocabulary))
      cat(paste0("\nModel <s> tags: ", private$..parameters$bosTags))

      return(TRUE)

    },
    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    validateParams = function(train, modelSize, k, gtDiscount, estimateGT,
                              fixedDiscount, openVocabulary) {

      private$..params <- list()
      private$..params$classes$name <- list('train')
      private$..params$classes$objects <- list(train)
      private$..params$classes$valid <- list('Corpus')
      private$..params$range$variable <- c('modelSize', 'fixedDiscount')
      private$..params$range$value <- c(modelSize, fixedDiscount)
      private$..params$range$low <- c(1,0.1)
      private$..params$range$high <- c(5, 0.95)
      private$..params$logicals$variables <- c('openVocabulary',
                                               "gtDiscount",
                                               "estimateGT")
      private$..params$logicals$values <- c(openVocabulary,
                                            gtDiscount,
                                            estimateGT)
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
    initialize = function(train, modelSize = 3, k = 5, gtDiscount = FALSE,
                          fixedDiscount = 0.5, openVocabulary = TRUE,
                          estimateGT = FALSE, bosTags = TRUE) {

      name <- paste0("Katz ",
                     private$..constants$modelTypes[modelSize], " Model")

      private$loadServices(name)

      private$validateParams(train, modelSize, k, gtDiscount, estimateGT,
                             fixedDiscount, openVocabulary)

      # Update settings
      private$..parameters$modelId <- private$meta$get(key = 'id')
      private$..parameters$modelName <- name
      private$..parameters$modelSize <- modelSize
      private$..parameters$k <- k
      private$..parameters$gtDiscount <- gtDiscount
      private$..parameters$fixedDiscount <- fixedDiscount
      private$..parameters$algorithm <- 'Katz'
      private$..parameters$modelType <- private$..constants$modelTypes[modelSize]
      private$..parameters$openVocabulary <- openVocabulary
      private$..parameters$vocabulary <- ifelse(openVocabulary == TRUE,
                                                'Open', 'Closed')
      private$..parameters$bosTags <- bosTags

      # Update meta data
      private$meta$set(key = 'algorithm', value = 'Katz', type = 'f')
      private$meta$set(key = 'openVocabulary', value = openVocabulary, type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType',
                       value = private$..parameters$modelType,
                       type = 'f')
      private$meta$set(key = 'k', value = k, type = 'f')
      private$meta$set(key = 'estimateGT', value = estimateGT, type = 'f')
      private$meta$set(key = 'gtDiscount', value = gtDiscount, type = 'f')
      private$meta$set(key = 'fixedDiscount', value = fixedDiscount, type = 'f')

      private$..corpora$train <- Clone$new()$this(x = train, reference = TRUE,
                                                  content = TRUE)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                Fit Method                               #
    #-------------------------------------------------------------------------#
    fit = function() {

      # Note start time
      private$startTime()

      # Build language model
      private$buildModel()

      # Note end time
      private$endTime()

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

      # Note start time
      private$startTime(train = FALSE)

      private$..corpora$test <- Clone$new()$this(x = test, reference = TRUE,
                                                 content = TRUE)
      # Build test set evaluation nGram tables
      private$buildEval()

      # Score
      private$score()

      # Note end time
      private$endTime(train = FALSE)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$katz(self)
    }
  )
)
