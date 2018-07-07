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
    discounts = function() {
      private$..model$discounts <- private$..model$totals$n1 /
        (private$..model$totals$n1 + 2 * private$..model$totals$n2)
      return(TRUE)
    },

    totals = function() {

      modelSize <- private$..settings$modelSize
      modelTypes <- private$..settings$modelTypes

      for (i in 1:modelSize) {

        # Summarize counts and store in summary table
        nGram <- modelTypes[i]
        n <- nrow(private$..model$nGrams[[i]])

        # Obtain frequency spectrum
        spectrum <- as.data.frame(table(private$..model$nGrams[[i]]$cNGram), stringsAsFactors = FALSE)
        names(spectrum) <- c('cNGram', 'freq')
        spectrum$cNGram <- as.numeric(spectrum$cNGram)
        for (j in 1:2) {
          if (is.na(spectrum[j,]$freq))  {
            spectrum <- rbind(spectrum, data.frame(cNGram = j, freq = 0))
          }
        }

        dt_i <- data.table(nGram = nGram, n = n,
                           n1 = spectrum$freq[1],
                           n2 = spectrum$freq[2])
        private$..model$totals <- rbind(private$..model$totals, dt_i)
      }
      return(TRUE)
    },


    prefix = function(n) {
      # Compute the number of times a prefix occurs in the corpus
      private$..model$nGrams[[n]][, ":=" (cPre = sum(cNGram)), by = prefix]
    },

    hist = function(n) {
      # Compute the number of histories in which the prefix of an
      # nGram occurs, e.g, the number of unique words that follow
      # an nGram prefix.  This computed for all levels except
      # the unigram level, which does not have a prefix.
      N1pPre_ <-
        private$..model$nGrams[[n]][,.(N1pPre_ = .N), by = .(prefix)]

      private$..model$nGrams[[n]] <-
        merge(private$..model$nGrams[[n]], N1pPre_, by = 'prefix',
              all.x = TRUE)
    },

    ckn = function(n) {
      # Compute continuation counts e.g. the number of words that
      # precede an nGram.  This is compute for all levels except
      # the highest.

      modelSize <- private$..settings$modelSize

      if (n < modelSize) {

        # Compute the continuation count for the nGram
        higher <- private$..model$nGrams[[n+1]][,.(suffix)]
        higher <- higher[,.(cKN_nGram = .N), by = .(suffix)]
        higher$cKN_nGram <- as.numeric(higher$cKN_nGram)
        setkey(private$..model$nGrams[[n]], nGram)
        setkey(higher, suffix)
        private$..model$nGrams[[n]] <-
          merge(private$..model$nGrams[[n]], higher, by.x = 'nGram',
                by.y = 'suffix', all.x = TRUE)

        # Handle special case where nGram is sequence of BOS tags
        # The continuation count is the number of occurences
        # of BOS tag.
        private$..model$nGrams[[n]][like(nGram, "BOS"), cKN_nGram := as.numeric(cNGram)]

      } else {
        private$..model$nGrams[[n]]$cKN_nGram <- private$..model$nGrams[[n]]$cNGram
      }

      return(TRUE)
    },

    counts = function() {

      modelSize <- private$..settings$modelSize

      for (n in 1:modelSize) {

        private$ckn(n)
        if (n > 1) {
          private$hist(n)
        }
        if (n == modelSize) private$prefix(n)

        for (i in seq_along(private$..model$nGrams[[n]])) {
          set(private$..model$nGrams[[n]],
              i=which(is.na(private$..model$nGrams[[n]][[i]])), j=i, value=0)
        }
      }
      private$totals()
      return(TRUE)
    },

    createTable = function(nGrams, n) {
      nGrams <- as.data.frame(table(nGrams), stringsAsFactors = FALSE)
      dt <- data.table(nGram = nGrams[,1],
                       cNGram = nGrams[,2])
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

      # Product nGrams
      train <- private$..corpora$train

      private$..model$nGrams <- lapply(seq(1:modelSize), function(n) {
        corpus <- Token$new(train)$nGrams('tokenizer', n)$getTokens()
        documents <- corpus$getDocuments()
        nGrams <- unlist(lapply(documents, function(d) {d$content}))
        private$createTable(nGrams, n)
      })

      names(private$..model$nGrams) <- modelTypes[1:modelSize]

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                       Model Estimation Methods                          #
    #-------------------------------------------------------------------------#
    alpha = function() {

      for (i in 1:private$..settings$modelSize) {

        if (i == 1) {
          private$..model$nGrams[[i]]$alpha <- private$..model$nGrams[[i]]$cKN_nGram /
            private$..model$totals$n[i+1]

        } else if (i < private$..settings$modelSize) {
          private$..model$nGrams[[i]]$alpha <-
            pmax(private$..model$nGrams[[i]]$cKN_nGram - private$..model$discounts[i],0) /
            private$..model$totals$n[i+1]

        } else {
          private$..model$nGrams[[i]]$alpha <-
            pmax(private$..model$nGrams[[i]]$cNGram - private$..model$discounts[i],0) /
            private$..model$nGrams[[i]]$cPre
        }
      }
    },

    lambda = function() {
      for (i in 2:private$..settings$modelSize) {

        if (i < private$..settings$modelSize) {
          private$..model$nGrams[[i]]$lambda <-
            private$..model$discounts[i] /
            private$..model$totals$n[i+1] *
            private$..model$nGrams[[i]]$N1pPre_
        } else {
          private$..model$nGrams[[i]]$lambda <-
            private$..model$discounts[i] /
            private$..model$nGrams[[i]]$cPre *
            private$..model$nGrams[[i]]$N1pPre_
        }
      }
    },

    pSmooth = function() {
      for (i in 1:private$..settings$modelSize) {

        if (i == 1) {
          private$..model$nGrams[[i]]$pSmooth <- private$..model$nGrams[[i]]$alpha

        } else {
          lower <- private$..model$nGrams[[i-1]][,.(nGram, pSmooth)]
          setnames(lower, "pSmooth", "pSmoothSuffix")
          setkey(lower, nGram)
          setkey(private$..model$nGrams[[i]], suffix)
          private$..model$nGrams[[i]] <-
            merge(private$..model$nGrams[[i]], lower, by.x = 'suffix',
                  by.y = 'nGram', all.x = TRUE)
          for (j in seq_along(private$..model$nGrams[[i]])) {
            set(private$..model$nGrams[[i]],
                i=which(is.na(private$..model$nGrams[[i]][[j]])), j=j, value=0)
          }
          private$..model$nGrams[[i]]$pSmooth <-
            private$..model$nGrams[[i]]$alpha +
            private$..model$nGrams[[i]]$lambda *
            private$..model$nGrams[[i]]$pSmoothSuffix
        }
      }
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
      private$prepCorpora()

      # Build ngram tables with raw frequency counts
      private$buildTables()

      # Add prefix and continuation counts
      private$counts()

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
