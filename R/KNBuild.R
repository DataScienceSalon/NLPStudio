#==============================================================================#
#                               KNBuild                                       #
#==============================================================================#
#' KNBuild
#'
#' \code{KNBuild} Computes counts used to estimate Kneser Ney probabilities
#'
#' Computes counts used in the probability calculations for the Kneser Ney
#' probabilities.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family SLMStudio Classes
#' @export
KNBuild <- R6::R6Class(
  classname = "KNBuild",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(
    ..model = list(
      nGrams = list(),
      totals = data.table(),
      discounts = data.table()
    ),

    discounts = function() {
      private$..model$discounts <- private$..model$totals$n1 /
        (private$..model$totals$n1 + 2 * private$..model$totals$n2)
      return(TRUE)
    },

    totals = function() {

      modelSize <- private$..config$getModelSize()
      modelTypes <- private$..config$getModelTypes()

      for (i in 1:modelSize) {

        # Summarize counts and store in summary table
        nGram <- .modelTypes[i]
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

      modelSize <- private$..config$getModelSize()

      if (n < modelSize) {

        # Compute the continuation count for the nGram
        higher <- private$..model$nGrams[[n+1]][,.(suffix)]
        higher <- higher[,.(cKN_nGram = .N), by = .(suffix)]
        higher$cMKN_nGram <- as.numeric(higher$cKN_nGram)
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

      modelSize <- private$..config$getModelSize()

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
      modelSize <- private$..config$getModelSize()
      modelTypes <- private$..config$getModelTypes()
      train <- private$..corpora$getTrain()

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
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(config, corpora) {

      private$loadServices()

      private$..params <- list()
      private$..params$classes$name <- list('config', 'corpora')
      private$..params$classes$objects <- list(config, corpora)
      private$..params$classes$valid <- list(c('SLMConfig', 'SLMCorpora'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..config <- config
      private$..corpora <- corpora

      event <-  paste0("Instantiated KNBuild ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    build = function() {

      # Build ngram tables with raw frequency counts
      private$buildTables()

      # Add prefix and continuation counts
      private$counts()

      # Compute discounts
      private$discounts()

      invisible(self)
    },

    getModel = function() {
      model <- KN$new(config = private$..config, model = private$..model)
      return(model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knBuild(self)
    }
  )
)
