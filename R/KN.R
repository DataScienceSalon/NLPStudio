#==============================================================================#
#                               Kneser Ney                                     #
#==============================================================================#
#' Kneser Ney
#'
#' \code{KN} Kneser-Ney Statistical Learning Model
#'
#' Encapsulates a Statistical Language Model implementing the Kneser-Ney
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
KN <- R6::R6Class(
  classname = "KN",
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
      discount <- private$..model$discounts[n]
      n1pPre_ <- max(nrow(private$..model$nGrams[[n]] %>% filter(prefix == pfx)), 1)
      if (n < private$..parameters$modelSize) {
        n1p_pre_ <- nrow(private$..model$nGrams[[n+1]])
        lambda <- discount / n1p_pre_ * n1pPre_
      } else {
        cPrefix <- max(as.numeric(private$..model$nGrams[[n-1]] %>% filter(nGram == pfx) %>%
          select(cNGram)), 1)
        lambda <- discount / cPrefix * n1pPre_
      }
      return(lambda)
    },

    #-------------------------------------------------------------------------#
    #                                 alpha                                   #
    #               Computes discounted probability for an nGram              #
    #-------------------------------------------------------------------------#
    alpha = function(ngram, pfx, n) {
      discount <- private$..model$discounts[n]
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
    #                                 pKN                                     #
    #               Computes Kneser-Ney probability of an nGram               #
    #-------------------------------------------------------------------------#
    pKN = function(ngram, n) {

      if (n == 1) {
        pKN <- nrow(private$..model$nGrams[[n+1]] %>% filter(suffix == ngram)) /
          nrow(private$..model$nGrams[[n+1]])
        if (pKN == 0) {
          private$..evaluation$performance$oov <-
            private$..evaluation$performance$oov + 1
        }

      } else {
        # Split nGram into prefix and suffix
        pfx <- gsub(private$..settings$regex$prefix[[n-1]], "\\1", ngram, perl = TRUE)
        sfx <- gsub(private$..settings$regex$suffix[[n-1]], "\\1", ngram, perl = TRUE)
        alpha <- private$alpha(ngram, pfx, n)
        lambda <- private$lambda(pfx, n)
        pKN <- alpha + (lambda * private$pKN(sfx, n-1))
      }
      return(pKN)
    },

    #-------------------------------------------------------------------------#
    #                                score                                    #
    #               Prepare perplexity scores for test set                    #
    #-------------------------------------------------------------------------#
    score = function() {

      private$initTestTables()

      nGrams <- private$..evaluation$scores$nGram
      scores <- rbindlist(lapply(nGrams, function(nGram) {
        p <- list()
        p$p <- private$pKN(nGram, n = private$..parameters$modelSize)
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
    # Computes discounts based upon the number of nGrams that occur once and  #
    # twice in the corpus                                                     #
    #-------------------------------------------------------------------------#
    discounts = function() {
      private$..model$discounts <- private$..model$totals$n1 /
        (private$..model$totals$n1 + 2 * private$..model$totals$n2)
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                                totals                                   #
    # Computes total nGrams and total nGrams occuring once and twice in the   #
    # corpus                                                                  #
    #-------------------------------------------------------------------------#
    totals = function() {

      modelSize <- private$..parameters$modelSize
      modelTypes <- private$..settings$modelTypes

      private$..model$totals <- rbindlist(lapply(seq(1:modelSize), function(i) {
        totals <- list()
        totals$nGram <- modelTypes[i]
        totals$n <- nrow(private$..model$nGrams[[i]])
        totals$n1 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 1))
        totals$n2 <- nrow(private$..model$nGrams[[i]] %>% filter(cNGram == 2))
        totals
      }))
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
    initialize = function(train, modelSize = 3,  openVocabulary = TRUE) {

      name <- paste0("Kneser-Ney ",
                     private$..settings$modelTypes[modelSize], " Model")

      private$loadServices(name)

      private$validateParams(train, modelSize, openVocabulary)

      # Update settings
      private$..parameters$modelId <- private$meta$get(key = 'id')
      private$..parameters$modelName <- name
      private$..parameters$modelSize <- modelSize
      private$..parameters$algorithm <- 'Kneser-Ney'
      private$..parameters$modelType <- private$..settings$modelTypes[modelSize]
      private$..parameters$vocabulary <- ifelse(openVocabulary == TRUE,
                                            'Open', 'Closed')

      # Update meta data
      private$meta$set(key = 'algorithm', value = 'Kneser-Ney', type = 'f')
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
      visitor$kn(self)
    }
  )
)
