#==============================================================================#
#                               KNCounts                                       #
#==============================================================================#
#' KNCounts
#'
#' \code{KNCounts} Computes counts used to estimate Kneser Ney probabilities
#'
#' Computes counts used in the probability calculations for the Kneser Ney
#' probabilities.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family LMStudio Classes
#' @export
KNCounts <- R6::R6Class(
  classname = "KNCounts",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(
    ..modelSize = numeric(),
    ..modelTypes = character(),
    ..nGrams = list(),
    ..totals = data.table(),
    ..discounts = data.table(),

    discounts = function() {
      private$..discounts <- private$..totals$n1 /
        (private$..totals$n1 + 2 * private$..totals$n2)
      return(TRUE)
    },

    totals = function() {

      for (i in 1:private$..modelSize) {

        # Summarize counts and store in summary table
        nGram <- private$..modelTypes[i]
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

      train <- private$..model$getTrain()
      document <- train$getDocuments()[[1]]

      private$..nGrams <- lapply(seq(1:private$..modelSize), function(n) {
        nGrams <- unlist(NLPStudio::tokenize(document$content, tokenType = 'word',
                                             nGrams = n, lowercase = FALSE))
        private$createTable(nGrams, n)
      })

      names(private$..nGrams) <- private$..modelTypes[1:private$..modelSize]

      return(TRUE)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('KNModel'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..model <- x
      private$..modelSize <- x$getModelSize()
      private$..modelTypes <- x$getModelTypes()

      event <-  paste0("Instantiated KNCounts ")
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

      # Update language model
      private$..model$setNGrams(private$..nGrams)
      private$..model$setDiscounts(private$..discounts)
      private$..model$setTotals(private$..totals)

      return(private$..model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knCounts(self)
    }
  )
)
