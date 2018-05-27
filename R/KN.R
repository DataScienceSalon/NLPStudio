#==============================================================================#
#                                   KN                                         #
#==============================================================================#
#' KN
#'
#' \code{KN} Kneser Ney language model object.
#'
#' Kneser Ney language model object.
#'
#' @param x Corpus object upon which the model will be trained.
#' @param size Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @export
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..train = character(),
    ..modelSize = numeric(),
    ..modelType = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),
    ..nGrams = list(),
    ..discounts = numeric(),
    ..totals = data.table(),

    #-------------------------------------------------------------------------#
    #                           Evaluation Methods                            #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                          Computation Methods                            #
    #-------------------------------------------------------------------------#
    alpha = function() {

      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$alpha <- private$..nGrams[[i]]$cKN_nGram /
            private$..totals$n[i+1]

        } else if (i < private$..modelSize) {
          private$..nGrams[[i]]$alpha <-
            pmax(private$..nGrams[[i]]$cKN_nGram - private$..discounts[i],0) /
            private$..totals$n[i+1]

        } else {
          private$..nGrams[[i]]$alpha <-
            pmax(private$..nGrams[[i]]$cNGram - private$..discounts[i],0) /
            private$..nGrams[[i]]$cPre
        }
      }
    },

    lambda = function() {
      for (i in 2:private$..modelSize) {

        if (i < private$..modelSize) {
          private$..nGrams[[i]]$lambda <-
            private$..discounts[i] /
            private$..totals$n[i+1] *
            private$..nGrams[[i]]$N1pPre_
        } else {
          private$..nGrams[[i]]$lambda <-
            private$..discounts[i] /
            private$..nGrams[[i]]$cPre *
            private$..nGrams[[i]]$N1pPre_
        }
      }
    },

    pKN = function() {
      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$pKN <- private$..nGrams[[i]]$alpha

        } else {
          lower <- private$..nGrams[[i-1]][,.(nGram, pKN)]
          setnames(lower, "pKN", "pKNSuffix")
          private$..nGrams[[i]] <-
            merge(private$..nGrams[[i]], lower, by.x = 'suffix',
                  by.y = 'nGram', all.x = TRUE)
          for (j in seq_along(private$..nGrams[[i]])) {
            set(private$..nGrams[[i]],
                i=which(is.na(private$..nGrams[[i]][[j]])), j=j, value=0)
          }
          private$..nGrams[[i]]$pKN <-
            private$..nGrams[[i]]$alpha +
            private$..nGrams[[i]]$lambda *
            private$..nGrams[[i]]$pKNSuffix
        }
      }
    },

    #-------------------------------------------------------------------------#
    #                           Count Methods                                 #
    #-------------------------------------------------------------------------#
    totals = function() {

      for (i in 1:private$..modelSize) {

        # Summarize counts and store in summary table
        nGram <- private$..modelType[i]
        n <- nrow(private$..nGrams[[i]])

        # Obtain frequency spectrum
        spectrum <- as.data.frame(table(private$..nGrams[[i]]$cNGram), stringsAsFactors = FALSE)
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
        private$..totals <- rbind(private$..totals, dt_i)
      }
      return(TRUE)
    },


    prefix = function(n) {
      # Compute the number of times a prefix occurs in the corpus
      private$..nGrams[[n]][, ":=" (cPre = sum(cNGram)), by = prefix]
    },


    hist = function(n) {
      # Compute the number of histories in which the prefix of an
      # nGram occurs, e.g, the number of unique words that follow
      # an nGram prefix.  This computed for all levels except
      # the unigram level, which does not have a prefix.
      N1pPre_ <-
        private$..nGrams[[n]][,.(N1pPre_ = .N), by = .(prefix)]

      private$..nGrams[[n]] <-
        merge(private$..nGrams[[n]], N1pPre_, by = 'prefix',
              all.x = TRUE)
    },

    ckn = function(n) {
      # Compute continuation counts e.g. the number of words that
      # precede an nGram.  This is compute for all levels except
      # the highest.

      if (n < private$..modelSize) {

        # Compute the continuation count for the nGram
        higher <- private$..nGrams[[n+1]][,.(suffix)]
        higher <- higher[,.(cKN_nGram = .N), by = .(suffix)]
        private$..nGrams[[n]] <-
          merge(private$..nGrams[[n]], higher, by.x = 'nGram',
                by.y = 'suffix', all.x = TRUE)

      } else {
        private$..nGrams[[n]]$cKN_nGram <- private$..nGrams[[n]]$cNGram
      }

      return(TRUE)
    },

    counts = function() {

      for (n in 1:private$..modelSize) {

        private$ckn(n)
        if (n > 1) {
          private$hist(n)
        }
        if (n == private$..modelSize) private$prefix(n)

        for (i in seq_along(private$..nGrams[[n]])) {
          set(private$..nGrams[[n]],
              i=which(is.na(private$..nGrams[[n]][[i]])), j=i, value=0)
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

      text <- private$..train$getDocuments()[[1]]$content

      private$..nGrams <- lapply(seq(1:private$..modelSize), function(n) {
        nGrams <- NLPStudio::tokenize(text, nGrams = n)
        private$createTable(nGrams, n)
      })

      names(private$..nGrams) <- private$..modelType[1:private$..modelSize]

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()
      if (self$is.openVocabulary())  {
        vocabulary <- 'Open'
      } else {
        vocabulary <- 'Closed'
      }
      NLPStudio::printHeading(text = paste0(meta$functional$smoothing, ": ",
                                            self$getName(), " Summary"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nId         : ", meta$identity$id))
      cat(paste0("\nName       : ", meta$identity$name))
      cat(paste0("\nType       : ", meta$functional$modelType))
      cat(paste0("\nSmoothing  : ", meta$functional$smoothing))
      cat(paste0("\nVocabulary : ", vocabulary))
      return(TRUE)

    },

    nGramSummary = function() {

      NLPStudio::printHeading(text = 'nGram Summary', symbol = "-", newlines = 2)
      print(private$..totals)
      return(TRUE)

    },

    discountSummary = function() {

      NLPStudio::printHeading(text = 'Discounts', symbol = "-", newlines = 2)
      print(private$..discounts)
      return(TRUE)

    },

    nGramDetail = function() {

      for (i in 1:private$meta$get(key = 'modelSize')) {
        meta <- private$meta$get()
        NLPStudio::printHeading(text = paste0(meta$functional$smoothing, ": ",
                                              private$..modelType[i], " Summary"),
                                symbol = "-",
                                newlines = 2)
        print(private$..nGrams[[i]][, tail(.SD, 10), by=cKN_nGram])
      }
      return(TRUE)
    },

    validate = function(x) {

      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      return(v)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(modelSize = 3) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)

      # Validation
      private$..params <- list()
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Initialize private members
      private$meta$set(key = 'smoothing', value = "Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType', value = private$..modelType[size], type = 'f')

      # Create log entry
      event <- paste0("KN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    build = function(x) {

      # Validate training set
      v <- private$validate(x)
      if (v$code == FALSE) {
        private$logR$log(method = 'build', event = v$msg, level = "Error")
        stop()
      }
      private$..train <- x
      private$buildTables()   # Build ngram tables with raw frequency counts
      private$counts()        # Add prefix and continuation counts
      private$discounts()     # Compute discounts
      private$alpha()         # Compute Alpha
      private$lambda()        # Compute lambda
      private$pKN()           # Compute probabilities

      return(private$..lm)
    },

    evaluate = function(x) {

      # Validate test set
      v <- private$validate(x)
      if (v$code == FALSE) {
        private$logR$log(method = 'evaluate', event = v$msg, level = "Error")
        stop()
      }

      private$..test <- x
      private$scoreTestData()
      private$perplexity()

    },


    summary = function() {
      private$overview()
      private$..document$summary(section = c("i", "q"))
      private$discountSummary()
      private$nGramSummary()
      private$nGramDetail()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }

  )
)
