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

    #-------------------------------------------------------------------------#
    #                             EVALUATE METHODS                            #
    #-------------------------------------------------------------------------#
    #-------------------------------------------------------------------------#
    #                                 cNGram                                  #
    #                      Obtain counts of test nGram                        #
    #-------------------------------------------------------------------------#
    cNGrams =function(i) {
      cNGrams <- private$..model$nGrams[[i]] %>% select(nGram, cNGram)
      private$..evaluation$nGrams[[i]] <-
          merge(private$..evaluation$nGrams[[i]],
                cNGrams, by = 'nGram')
        private$..evaluation$nGrams[[i]][is.na(private$..evaluation$nGrams[[i]])] <- 0
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                 qML                                     #
    #           Compute unigram maximum likelihood probabilities              #
    #-------------------------------------------------------------------------#
    qML = function(i) {
      if (i == 1) {
        cNGramTtl = as.numeric(private$..evaluation$nGrams[[i]] %>%
          summarise(cNGramTtl = sum(cNGram)) %>%
          select(cNGramTtl))
        private$..evaluation$nGrams[[i]]$qML <-
          private$..evaluation$nGrams[[i]]$cNGram / cNGramTtl
      }
    },

    #-------------------------------------------------------------------------#
    #                                cKatz                                    #
    #                 Discounted Counts for Observed Unigrams                 #
    #-------------------------------------------------------------------------#
    cKatz = function(i) {

      if (i > 1) {

        # Apply Good-Turing discount unless the fixedDiscount parameter is TRUE
        if (private$..parameters$gtDiscount) {
          discounts <- as.numeric(private$..model$discounts[i])
          private$..evaluation$nGrams[[i]] <- private$..evaluation$nGrams[[i]] %>%
            mutate(cKatz = ifelse (cNGram > 5,
                                    cNGram,
                                    cNGram *
                                     discounts[cNGram]))
        } else {
          private$..evaluation$nGrams[[i]] <- private$..evaluation$nGrams[[i]] %>%
            mutate(cKatz = ifelse (cNGram > 5,
                                    cNGram,
                                    cNGram -
                                     private$..parameters$fixedDiscount))

        }

        # Clean up NA and negative values by setting cKatz to zero.
        private$..evaluation$nGrams[[i]]$cKatz <- ifelse(is.na(private$..evaluation$nGrams[[i]]$cKatz), 0,
                                                    ifelse(private$..evaluation$nGrams[[i]]$cKatz < 0,0,
                                                           private$..evaluation$nGrams[[i]]$cKatz))
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                               cPrefix                                   #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    cPrefix = function(i) {

      if (i > 1) {
        lower <- private$..evaluation$nGrams[[i-1]][,c("nGram", "cNGram")]
        names(lower) <- c("nGram", "cPrefix")
        private$..evaluation$nGrams[[i]] <- merge(private$..evaluation$nGrams[[i]],
                                             lower, by.x = "prefix",
                                             by.y = "nGram", all.x = TRUE)

        # Count start of sentence tokens
        private$..evaluation$nGrams[[i]][ prefix == paste(rep("BOS", (i-1)), collapse = " "),
               cPrefix := private$..corporaStats$train$sentences]
      }
      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                                 qBOA                                    #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    qBOA = function() {

      if (i > 1) {

        private$..evaluation$nGrams[[i]]$qBOA <-
          private$..evaluation$nGrams[[i]]$cKatz /
          private$..evaluation$nGrams[[i]]$cPrefix
        private$..evaluation$nGrams[[i]]$qBOA <-
          ifelse(is.na(private$..evaluation$nGrams[[i]]$qBOA), 0,
                 private$..evaluation$nGrams[[i]]$qBOA)

      }

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                 Alpha                                   #
    #       Compute alpha, the probability mass taken by discounting          #
    #-------------------------------------------------------------------------#
    alpha = function(i) {

      if (i > 1) {

        cKatzPfxSum <- private$..evaluation$nGrams[[i]] %>% group_by(prefix) %>%
          summarise(cKatzPfxSum = sum(cKatz)) %>% select(prefix, cKatzPfxSum)
        names(cKatzPfxSum) <- c("prefix", "cKatzPfxSum")

        # Add cKatzPrefix, the sum of the discounted counts of seen nGrams by prefix
        private$..evaluation$nGrams[[i]] <- merge(private$..evaluation$nGrams[[i]],
                                                  cKatzPfxSum, by = "prefix", all.x = TRUE)

        # Replace NAs with zero
        private$..evaluation$nGrams[[i]]$cKatzPfxSum <-
          ifelse(is.na(private$..evaluation$nGrams[[i]]$cKatzPfxSum), 0,
                 private$..evaluation$nGrams[[i]]$cKatzPfxSum)

        # Compute alpha
        private$..evaluation$nGrams[[i]] <-
          private$..evaluation$nGrams[[i]] %>%
          mutate(alpha = 1 - (cKatzPfxSum / cPrefix))

      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                lambda                                   #
    # Compute lambda, the redistribution of discounted probability mass       #
    #-------------------------------------------------------------------------#
    lambda = function(i) {

      if (i > 1) {

        # Compute lambda numerator fior each nGram suffix
        if (i == 2) {
          qBOBSfx <- private$..evaluation$nGrams[[i-1]] %>% select(nGram, pML)
        } else {
          qBOBSfx <- private$..evaluation$nGrams[[i-1]] %>% select(nGram, qBO)
        }
        names(qBOBSfx) <- c("suffix", "qBOBSfx")
        private$..evaluation$nGrams[[i]] <- merge(private$..evaluation$nGrams[[i]],
                                                  qBOBSfx, by = "suffix",
                                                  all.x = TRUE)

        # Compute lambda denominator
        qBOBSfxSum <- private$..evaluation$nGrams[[i]] %>% group_by(prefix) %>%
          summarise(qBOBSfxSum = (1 - sum(qBOBSfx))) %>% select(prefix, qBOBSfxSum)

        private$..evaluation$nGrams[[i]] <- merge(private$..evaluation$nGrams[[i]],
                                                  qBOBSfxSum, by = 'prefix',
                                                  all.x = TRUE)

        # Compute lamba
        private$..evaluation$nGrams[[i]]$lambda <-
          private$..evaluation$nGrams[[i]]$qBOBSfx /
          private$..evaluation$nGrams[[i]]$qBOBSfxSum
      }
      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                                qBOB                                     #
    #         Compute nGram probability based upon unobserved nGrams          #
    #-------------------------------------------------------------------------#
    qBOB = function(i) {

      if (i > 1) {

        private$..evaluation$nGrams[[i]] <-
          private$..evaluation$nGrams[[i]] %>% mutate(qBOB = (alpha * lambda))

      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                qBO                                      #
    #                  Compute final backoff probability                      #
    #-------------------------------------------------------------------------#
    qBO = function(i) {

      if (i > 1) {
        private$..evaluation$nGrams[[i]]$qBO <-
          ifelse(private$..evaluation$nGrams[[i]]$qBOA == 0,
                 private$..evaluation$nGrams[[i]]$qBOB,
                 private$..evaluation$nGrams[[i]]$qBOA)
      }
      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                            evalTables                                   #
    #      Builds evaluation tables which store counts for the test data      #
    #-------------------------------------------------------------------------#
    evalTables = function(){

      private$prepTest()
      private$..evaluation$nGrams <-
        private$initNGrams(private$..corpora$test, count = FALSE)

      for (i in 1:private$..parameters$modelSize) {
        private$cNGrams(i)
        private$qML(i)
        private$cKatz(i)
        private$cPrefix(i)
        private$qBOA(i)
        private$alpha(i)
        private$lambda(i)
        private$qBOB(i)
        private$qBO(i)
      }
      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Computes perplexity score for test data                   #
    #-------------------------------------------------------------------------#
    score = function(){


      # Extract backoff probabilities at highest nGram level
      private$..evaluation$scores <-
        private$..evaluation$nGrams[[private$..parameters$modelSize]] %>%
        select(nGram, qBO)
      names(private$..evaluation$scores) <- c("nGram", "p")

      # Clean up NA and inf values
      private$..evaluation$scores[is.infinite(p) | is.na(p), p := 0]

      # Count number of NGrams
      private$..evaluation$performance$nGrams <-
        nrow(private$..evaluation$scores) - 1

      # Compute oov rate
      private$..evaluation$performance$oovRate <-
        private$..evaluation$performance$oov /
        private$..evaluation$performance$nGrams

      # Count number of zero probabilities
      private$..evaluation$performance$zeroProbs <-
        nrow(private$..evaluation$scores %>% filter(p == 0))

      # Compute Zero Prob Rate
      private$..evaluation$performance$zeroProbRate <-
        private$..evaluation$performance$zeroProbs /
        private$..evaluation$performance$nGrams

      # Compute Log Probability
      private$..evaluation$performance$logProb <-
        sum(log(private$..evaluation$scores$p))

      # Compute perplexity
      private$..evaluation$performance$perplexity <-
        2^(-private$..evaluation$performance$logProb /
             private$..corporaStats$train$vocabulary)

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                eval                                     #
    #               Builds model on test data and scores                      #
    #-------------------------------------------------------------------------#
    eval = function(){

      private$evalTables()
      private$score()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             BUILD METHODS                               #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                              discounts                                  #
    #         Computes discount rates to be applied to raw nGram counts       #
    #-------------------------------------------------------------------------#
    discounts = function() {

      k <- private$..parameters$k
      n <- private$..parameters$modelSize

      private$..model$discounts <- rbindlist(lapply(seq(1:n), function(i) {

        nTotal <- as.numeric(private$..model$totals %>% filter(n == i) %>%
                               select(nGramTypes))
        d_r <- list()
        for (r in 1:k) {

          n_1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 1))
          if (private$..parameters$estimateGT) {
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
    # Build language model tables, including nGrams, totals and discounts.    #
    #-------------------------------------------------------------------------#
    build = function() {

      # Annotate text, initialize model tables and compute totals and discounts
      private$prepTrain()
      private$..model$nGrams <- private$initNGrams(private$..corpora$train)
      private$totals()
      private$discounts()

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
    initialize = function(train, modelSize = 3, k = 5,
                          gtDiscount = FALSE, estimateGT = TRUE,
                          fixedDiscount = 0.5, openVocabulary = TRUE,
                          bosTags = FALSE) {

      name <- paste0("Modified Kneser-Ney ",
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
      private$..parameters$estimateGT <- estimateGT
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

      # Build tables
      private$build()

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
      # Evaluate test set
      private$eval()

      # Note end time
      private$endTime(train = FALSE)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                            Predict Method                               #
    #-------------------------------------------------------------------------#
    predict = function(nGram) {

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$katz(self)
    }
  )
)
