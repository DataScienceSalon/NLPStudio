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
    #                             BUILD METHODS                               #
    #-------------------------------------------------------------------------#
    #-------------------------------------------------------------------------#
    #                                  pML                                    #
    #                 Conputes maximum likelihood of unigrams                 #
    #-------------------------------------------------------------------------#
    pML = function(i, nGrams) {

      if (i == 1) nGrams$pML <- nGrams$cNGram / sum(nGrams$cNGram)
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                                cKatz                                    #
    #                 Discounted Counts for Observed Unigrams                 #
    #-------------------------------------------------------------------------#
    cKatz = function(i, nGrams) {

      if (i > 1) {
        model <- private$..model$nGrams[[i]][,c("nGram", "cNGram")]
        if (private$..parameters$gtDiscount) {
          discounts <- as.numeric(private$..model$discounts[i])
          model <- model %>%
            mutate(cKatz = ifelse (cNGram > 5,
                                    cNGram,
                                    cNGram *
                                     discounts[cNGram])) %>%
            select(nGram, cKatz)
        } else {
          model <- model %>%
            mutate(cKatz = ifelse (cNGram > 5,
                                    cNGram,
                                    cNGram -
                                     private$..parameters$fixedDiscount)) %>%
            select(nGram, cKatz)

        }
      nGrams <- merge(nGrams, model, by = "nGram", all.x = TRUE)
      nGrams[ is.na(cKatz),  cKatz := 0]
      }
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                               cPrefix                                   #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    cPrefix = function(i, nGrams) {

      if (i > 1) {
        lower <- private$..model$nGrams[[i-1]][,c("nGram", "cNGram")]
        names(lower) <- c("nGram", "cPrefix")
        nGrams <- merge(nGrams,lower, by.x = "prefix",
                             by.y = "nGram", all.x = TRUE)

        # Count start of sentence tokens
        nGrams[ prefix == paste(rep("BOS", (i-1)), collapse = " "),
                cPrefix := private$..corporaStats$train$sentences]

        nGrams[ is.na(cPrefix),  cPrefix := 0]
      }
      return(nGrams)
    },
    #-------------------------------------------------------------------------#
    #                                 qBOA                                    #
    #                      Counts the nGram prefixes                          #
    #-------------------------------------------------------------------------#
    qBOA = function(i, nGrams) {

      if (i > 1) {

        nGrams$qBOA <- nGrams$cKatz / nGrams$cPrefix
        nGrams$qBOA <- ifelse(is.na(nGrams$qBOA), 0, nGrams$qBOA)

      }

      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                              Compute Alpha                              #
    #-------------------------------------------------------------------------#
    alpha = function(i, nGrams) {

      if (i > 1) {
        cKatzSum <- private$..model$nGrams[[i]] %>% group_by(prefix) %>%
          summarise(cKatzSum = sum(cKatz)) %>% select(prefix, cKatzSum)
        nGrams <- merge(nGrams, cKatzSum, by.x = 'prefix', by.y = 'prefix')
        nGrams$aPrefix <- 1 - (nGrams$cKatzSum / nGrams$cPrefix)
      }

      return(nGrams)
    },
    #-------------------------------------------------------------------------#
    #                               qBOB                                      #
    #            Computes sum of probabilities from unobserved nGrams         #
    #-------------------------------------------------------------------------#
    qBOB = function(i, nGrams) {

      if (i > 1) {

        # Compute numerator
        if (i == 2) {
          qBO <- private$..model$nGrams[[i-1]] %>% select(nGram, pML)
        } else {
          qBO <- private$..model$nGrams[[i-1]] %>% select(nGram, qBO)
        }
        names(qBO) <- c("nGram", "qBOSfx")
        nGrams <- merge(nGrams, qBO, by.x = 'suffix', by.y = 'nGram')

        # Compute denominator
        qBOSfxSum <- nGrams %>% group_by(prefix) %>%
          summarize(qBOSfxSum = sum(qBOSfx)) %>% select(prefix, qBOSfxSum)
        nGrams <- merge(nGrams, qBOSfxSum, by.x = 'prefix', by.y = 'prefix')

        # Compute redistributed probability mass (qRPM)
        nGrams$qRPM <-  1 - (nGrams$qBOSfx / nGrams$qBOSfxSum)

        # Compute qBOB
        nGrams$qBOB <- nGrams$aPrefix * nGrams$qRPM

      }
      return(nGrams)
    },

    #-------------------------------------------------------------------------#
    #                           scoreNGram                                    #
    #               Assigns a probability to each test nGram                  #
    #-------------------------------------------------------------------------#
    qBO = function(i, nGrams) {
      if (i > 1) nGrams$qBO <- ifelse(nGrams$qBOA > 0, nGrams$qBOA,
                                      nGrams$alpha * nGrams$qBOB)
      return(nGrams)
    },
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
    #         Driver method for computing and building the nGram tables.      #
    #-------------------------------------------------------------------------#
    build = function() {

      private$prepTrain()
      private$initModelTables()
      private$totals()
      private$discounts()

      for (i in 1:private$..parameters$modelSize) {

        private$..model$nGrams[[i]] <- private$pML(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$cKatz(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$cPrefix(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$qBOA(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$alpha(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$qBOB(i, private$..model$nGrams[[i]])
        private$..model$nGrams[[i]] <- private$qBO(i, private$..model$nGrams[[i]])
      }
      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                             EVALUATE METHODS                            #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Compute probabilities or each nGram                       #
    #-------------------------------------------------------------------------#
    score = function() {

      private$prepTest()
      private$initFitTables()
      for (i in 1:private$..parameters$modelSize) {
        if (i == 1) {
          private$..evaluation$nGrams[[i]] <-
            private$pML(private$..evaluation$nGrams[[i]])
        } else {
          private$..evaluation$nGrams[[i]] <- private$cKatz(i, private$..evaluation$nGrams[[i]])
          private$..evaluation$nGrams[[i]] <- private$cPrefix(i, private$..evaluation$nGrams[[i]])
          private$..evaluation$nGrams[[i]] <- private$qBOA(i, private$..evaluation$nGrams[[i]])
          private$..evaluation$nGrams[[i]] <- private$alpha(i, private$..evaluation$nGrams[[i]])
          private$..evaluation$nGrams[[i]] <- private$qBOB(i, private$..evaluation$nGrams[[i]],
                                                      private$..evaluation$nGrams[[i-1]])
          private$..evaluation$nGrams[[i]] <- private$qBO(i,private$..evaluation$nGrams[[i]])
        }
      }
      print(private$..evaluation$nGrams)

      # Extract backoff probabilities at highest nGram level
      private$..evaluation$scores <-
        private$..evaluation$nGrams[[private$..parameters$modelSize]] %>%
        select(nGram, qBO)
      names(private$..evaluation$scores) <- c("nGram", "p")

      # Clean up NA and inf values
      private$..evaluation$scores[is.infinite(p) | is.na(p), p := 0]

      # Count number of NGrams
      private$..evaluation$performance$nGrams <-
        nrow(private$..evaluation$scores)

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
                          fixedDiscount = 0.5, openVocabulary = TRUE) {

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

      # Prepare test corpus
      private$prepTest()

      # Score test set
      private$score()

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
