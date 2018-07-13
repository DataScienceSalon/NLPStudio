#==============================================================================#
#                             Modified Kneser Ney                              #
#==============================================================================#
#' Modified Kneser Ney
#'
#' \code{MKN} Modified Kneser-Ney Statistical Learning Model
#'
#' Encapsulates a Statistical Language Model implementing the Modified Kneser-Ney
#' smoothing algorithm.
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
MKN <- R6::R6Class(
  classname = "MKN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  private = list(
    #-------------------------------------------------------------------------#
    #                             SCORE METHODS                               #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                                 lambda                                  #
    #                       Computes backoff weight                           #
    #-------------------------------------------------------------------------#
    lambda = function(pfx, n)  {

      # Obtain discounts
      D1 <- as.numeric(private$..model$discounts[n,3])
      D2 <- as.numeric(private$..model$discounts[n,4])
      D3 <- as.numeric(private$..model$discounts[n,5])

      # Counts of nGrams with prefix, that occur 1, 2, and 3 ore more times
      n1Pre_ <- nrow(private$..model$nGrams[[n]] %>%
                        filter(prefix == pfx & cNGram == 1))
      n2Pre_ <- nrow(private$..model$nGrams[[n]] %>%
                       filter(prefix == pfx & cNGram == 2))

      n3pPre_ <- nrow(private$..model$nGrams[[n]] %>%
                       filter(prefix == pfx & cNGram > 2))

      if (n1Pre_ + n2Pre_ + n3pPre_ == 0) n1Pre_ <- 1

      numerator <- (D1 * n1Pre_) + (D2 * n2Pre_) + (D3 * n3pPre_)

      if (n < private$..parameters$modelSize) {

        denominator <- nrow(private$..model$nGrams[[n+1]])

      } else {
        denominator <- max(as.numeric(private$..model$nGrams[[n-1]] %>% filter(nGram == pfx) %>%
          select(cNGram)), 1)
      }

      lambda <- numerator / denominator

      return(lambda)
    },

    #-------------------------------------------------------------------------#
    #                              getDiscount                                #
    #               Computes discount based upon nGram count                  #
    #-------------------------------------------------------------------------#
    getDiscount = function(ngram, n) {

      idx <- min(as.numeric(private$..model$nGrams[[n]] %>% filter(nGram == ngram) %>%
        select(cNGram)), 3) + 2

      if (is.na(idx))  {
        discount <- 0
      } else {
        discount <- as.numeric(private$..model$discounts[n,..idx])
      }

      return(discount)
    },

    #-------------------------------------------------------------------------#
    #                                 alpha                                   #
    #               Computes discounted probability for an nGram              #
    #-------------------------------------------------------------------------#
    alpha = function(ngram, pfx, n) {

      discount <- private$getDiscount(ngram, n)

      if (n < private$..parameters$modelSize) {
        alpha <- max(nrow(private$..model$nGrams[[n+1]] %>%
                            filter(suffix == ngram)) - discount, 0) /
          nrow(private$..model$nGrams[[n+1]])
      } else {
        alpha <- max((as.numeric(private$..model$nGrams[[n]] %>% filter(nGram == ngram) %>%
                       select(cNGram))) - discount, 0) /
          as.numeric(private$..model$nGrams[[n-1]] %>% filter(nGram == pfx) %>%
          select(cNGram))
        if (is.na(alpha)) alpha <- 0
      }
      return(alpha)
    },

    #-------------------------------------------------------------------------#
    #                                 pMKN                                    #
    #           Computes Modified Kneser-Ney probability of an nGram          #
    #-------------------------------------------------------------------------#
    pMKN = function(ngram, n) {

      if (n == 1) {
        pMKN <- nrow(private$..model$nGrams[[n+1]] %>% filter(suffix == ngram)) /
          nrow(private$..model$nGrams[[n+1]])
        if (pMKN == 0) {
          private$..evaluation$performance$oov <-
            private$..evaluation$performance$oov + 1
        }

      } else {
        # Split nGram into prefix and suffix
        pfx <- gsub(private$..settings$regex$prefix[[n-1]], "\\1", ngram, perl = TRUE)
        sfx <- gsub(private$..settings$regex$suffix[[n-1]], "\\1", ngram, perl = TRUE)
        alpha <- private$alpha(ngram, pfx, n)
        lambda <- private$lambda(pfx, n)
        pMKN <- alpha + (lambda * private$pMKN(sfx, n-1))
      }
      return(pMKN)
    },

    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Prepare perplexity scores for test set                    #
    #-------------------------------------------------------------------------#
    score = function() {

      private$initScoresTable()

      nGrams <- private$..evaluation$scores$nGram
      scores <- rbindlist(lapply(nGrams, function(nGram) {
        p <- list()
        p$p <- private$pMKN(nGram, n = private$..parameters$modelSize)
        p
      }))

      private$..evaluation$scores <- cbind(private$..evaluation$scores, scores)
      private$prepEvalReport()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                             BUILD METHODS                               #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                               discounts                                 #
    # Computes discounts based upon the nGram counts                          #
    #-------------------------------------------------------------------------#
    discounts = function() {
      private$..model$discounts <-
        rbindlist(lapply(seq_along(1:private$..parameters$modelSize), function(i) {
        d <- list()
        d$D <- private$..model$totals$n1[i] /
          (private$..model$totals$n1[i] + 2 * private$..model$totals$n2[i])

        d$D0 <- 0

        d$D1 <- 1 - (2 * d$D *
                       private$..model$totals$n2[i] /
                       private$..model$totals$n1[i])

        d$D2 <- 2 - (3 * d$D *
                       private$..model$totals$n3[i] /
                       private$..model$totals$n2[i])

        d$D3 <- 3 - (4 * d$D *
                       private$..model$totals$n4[i] /
                       private$..model$totals$n3[i])
        d
      }))
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                totals                                   #
    # Computes total nGrams and total nGrams occuring once and twice in the   #
    # corpus                                                                  #
    #-------------------------------------------------------------------------#
    totals = function() {

      private$..model$totals <- data.table()

      for (i in 1:private$..parameters$modelSize) {

        # Summarize counts and store in summary table
        nGram <- private$..settings$modelTypes[i]
        n <- nrow(private$..model$nGrams[[i]])
        n1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 1))
        n2 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 2))
        n3 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 3))
        n4 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 4))

        dt_i <- data.table(nGram = nGram, n = n, n1 = n1, n2 = n2,
                           n3 = n3, n4 = n4)
        private$..model$totals <- rbind(private$..model$totals, dt_i)
      }
      return(TRUE)
    },


    #-------------------------------------------------------------------------#
    #                                build                                    #
    #         Driver method for computing and building the nGram tables.      #
    #-------------------------------------------------------------------------#
    build = function() {

      private$prepTrain()
      private$initNGramTables()
      private$totals()
      private$discounts()
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    validateParams = function(train, modelSize, openVocabulary) {

      private$..params <- list()
      private$..params$classes$name <- list('train')
      private$..params$classes$objects <- list(train)
      private$..params$classes$valid <- list('Corpus')
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- c(1)
      private$..params$range$high <- c(5)
      private$..params$logicals$variables <- c('openVocabulary')
      private$..params$logicals$values <- c(openVocabulary)
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
    initialize = function(train, modelSize = 3, epsilon = 10^-7,
                          openVocabulary = TRUE) {

      name <- paste0("Modified Kneser-Ney ",
                     private$..settings$modelTypes[modelSize], " Model")

      private$loadServices(name)

      private$validateParams(train, modelSize, openVocabulary)

      # Update settings
      private$..parameters$modelId <- private$meta$get(key = 'id')
      private$..parameters$modelName <- name
      private$..parameters$modelSize <- modelSize
      private$..parameters$algorithm <- 'Modified Kneser-Ney'
      private$..parameters$epsilon <- epsilon
      private$..parameters$modelType <- private$..settings$modelTypes[modelSize]
      private$..parameters$vocabulary <- ifelse(openVocabulary == TRUE,
                                                'Open', 'Closed')

      # Update meta data
      private$meta$set(key = 'algorithm', value = 'Modified Kneser-Ney', type = 'f')
      private$meta$set(key = 'openVocabulary', value = openVocabulary, type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType',
                       value = private$..settings$modelTypes[modelSize],
                       type = 'f')

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
      visitor$mkn(self)
    }
  )
)
