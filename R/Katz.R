#==============================================================================#
#                                   KN                                         #
#==============================================================================#
#' KN
#'
#' \code{KN} Kneser Ney Statistical Learning Model
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
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  private = list(

    #-------------------------------------------------------------------------#
    #                       Model Building Methods                            #
    #-------------------------------------------------------------------------#
    cStar = function() {

      k <- 5

      for (i in 2:private$..settings$modelSize) {

        # Obtain frequency spectrum
        s <- as.data.frame(table(private$..model$nGrams[[i]]$r))
        s$Var1 <- as.numeric(s$Var1)
        df <- data.frame(nGram = rep(n, nrow(s)),
                         r = s$Var1,
                         rPlus1 = s$Var1 + 1,
                         n_r = s$Freq)

        # Compute adjusted r count, rStar
        df2 <- df %>% select(r, n_r)
        df <- merge(df, df2,
                    by.x = c("rPlus1"),
                    by.y = c("r"))
        df <- data.frame(nGram = df$nGram,
                         r = df$r,
                         rPlus1 = df$rPlus1,
                         n_r = df$n_r.x,
                         n_rPlus1 = df$n_r.y)
        df$rStar <- df$rPlus1 * df$n_rPlus1 / df$n_r

        # Compute discount ratio
        n_kPLus1 <- as.numeric(df %>% filter(r == (k+1)) %>% select(n_r))
        n_1 <- as.numeric(df %>% filter(r == 1) %>% select(n_r))
        df$d_r <- ifelse(df$r > k, 1, (((df$rStar / df$r) - ((k+1)*n_kPLus1 / n_1)) /
                     1 - ((k+1) * n_kPLus1 / n_1)))

        # Add discount ratio to nGram and compute cKatz
        df <- df %>% select(r, d_r)
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                                             df,
                                             by = 'r')
        private$..model$nGrams[[i]]$cStar <- private$..model$nGrams[[i]]$r *
          private$..model$nGrams[[i]]$d_r
      }
      return(TRUE)
    },

    pKatz = function() {
      for (i in 2:private$..settings$modelSize) {
        lower <- private$..model$nGrams[[i-1]] %>% select(nGram, r)
        names(lower)[names(lower) == 'r'] <- 'cPrefix'
        private$..model$nGrams[[i]] <- merge(private$..model$nGrams[[i]],
                                             lower,
                                             by.x = 'prefix',
                                             by.y = 'nGram')
        private$..model$nGrams[[i]]$pKatz <- private$..model$nGrams[[i]]$cStar /
          private$..model$nGrams[[i]]$cPrefix
      }
    },

    pML = function() {
      private$..model$nGrams[[1]]$pML <-
        private$..model$nGrams[[1]]$r / sum(private$..model$nGrams[[1]]$r)
      return(TRUE)
    },


    createTable = function(nGrams, n) {
      nGrams <- as.data.frame(table(nGrams), stringsAsFactors = FALSE)
      dt <- data.table(nGram = nGrams[,1],
                       r = nGrams[,2])
      if (n > 1) {
        dt$prefix <- gsub(private$..regex$prefix[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$suffix  <- gsub(private$..regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)

      }
      return(dt)
    },

    buildTables = function() {

      # Obtain model parameters and training Corpus object.
      modelSize <- private$..settings$modelSize
      modelTypes <- private$..settings$modelTypes
      train <- private$..corpora$train

      document <- train$getDocuments()[[1]]

      private$..model$nGrams <- lapply(seq(1:modelSize), function(n) {
        nGrams <- unlist(NLPStudio::tokenize(document$content, tokenUnit = 'word',
                                             nGrams = n, lowercase = FALSE))
        private$createTable(nGrams, n)
      })

      names(private$..model$nGrams) <- modelTypes[1:modelSize]

      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function(train = NULL, test = NULL, name = NULL,
                           modelSize = 3, openVocabulary = TRUE) {

      private$loadServices(name)

      private$validateParams(train, test, name, modelSize, openVocabulary)

      # Update settings
      private$..settings$modelName <- name
      private$..settings$modelSize <- modelSize
      private$..settings$algorithm <- 'Kneser-Ney'
      private$..settings$modelType <- private$..settings$modelTypes[modelSize]
      private$..settings$openVocabulary <- openVocabulary

      # Update meta data
      private$meta$set(key = 'algorithm', value = 'Kneser-Ney', type = 'f')
      private$meta$set(key = 'openVocabulary', value = openVocabulary, type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType',
                       value = private$..settings$modelTypes[modelSize],
                       type = 'f')

      private$..corpora$train <- train
      private$..corpora$test <- test

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                Build Model                              #
    #-------------------------------------------------------------------------#
    fit = function() {

      # Create Training and Test Corpora for Modeling
      private$prepTrain()
      private$prepTest()

      # Build ngram tables with raw frequency counts
      private$buildTables()

      # Add prefix and continuation counts
      private$rStar()

      # Compute discounts
      private$discounts()

      # Compute pseudo probability alpha
      private$alpha()

      # Compute backoff weight lambda
      private$lambda()

      # Compute probabilities
      private$pSmooth()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }

  )
)
