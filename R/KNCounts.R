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

    discounts = function() {
      private$..discount <- private$..totals$n1 /
        (private$..totals$n1 + 2 * private$..totals$n2)
      return(TRUE)
    },

    totals = function() {

      private$..totals <- data.table()

      for (i in 1:private$..size) {

        # Summarize counts and store in summary table
        nGram <- private$..modelType[i]
        n <- nrow(private$..nGrams[[i]])
        totalCkn <- ifelse(i > 1,
                        sum(length(unique(private$..nGrams[[i]]$history))),
                        0)

        # Obtain frequency spectrum
        spectrum <- as.data.frame(table(private$..nGrams[[i]]$count), stringsAsFactors = FALSE)
        names(spectrum) <- c('count', 'freq')
        spectrum$count <- as.numeric(spectrum$count)
        for (j in 1:4) {
          if (is.na(spectrum[j,]$freq))  {
            spectrum <- rbind(spectrum, data.frame(count = j, freq = 0))
          }
        }

        dt_i <- data.table(nGram = nGram, n = n, totalCkn = totalCkn,
                           n1 = spectrum$freq[1],
                           n2 = spectrum$freq[2],
                           n3 = spectrum$freq[3],
                           n4 = spectrum$freq[4])
        private$..totals <- rbind(private$..totals, dt_i)
      }
      return(TRUE)
    },

    hist = function(n) {
      if (n > 1) {
        contextNHist <-
          private$..nGrams[[n]][,.(contextNHist = .N), by = .(context)]

        private$..nGrams[[n]] <-
          merge(private$..nGrams[[n]], contextNHist, by = 'context',
                all.x = TRUE)
      }
    },

    ckn = function(n) {

      # Compute continuation counts
      current <- private$..nGrams[[n]]

      if (n < private$..size) {

        higher <- private$..nGrams[[n+1]][,.(suffix)]
        higher <- higher[,.(cKN = .N), by = .(suffix)]
        current <- merge(current, higher, by.x = 'nGram',
                         by.y = 'suffix', all.x = TRUE)

        for (i in seq_along(current)) {
          set(current, i=which(is.na(current[[i]])), j=i, value=0)
        }
      } else {
        current <- current[,cKN := count]
      }
      private$..nGrams[[n]] <- current
      return(TRUE)
    },

    counts = function() {

      private$..totals <- data.table()

      for (n in 1:private$..size) {

        private$ckn(n)
        private$hist(n)

      }
      private$totals()
      return(TRUE)
    },

    createTable = function(ngrams, n) {
      ngrams <- as.data.frame(table(ngrams), stringsAsFactors = FALSE)
      dt <- data.table(nGram = ngrams[,1],
                       count = ngrams[,2])
      if (n > 1) {
        dt$history <- gsub( " .*$", "", dt$nGram)
        dt$context <- gsub(private$..regex$context[[n-1]], "\\1", dt$nGram, perl = TRUE)
        dt$suffix  <- gsub(private$..regex$suffix[[n-1]], "\\1", dt$nGram, perl = TRUE)

      }
      return(dt)
    },

    createNGrams = function(text, tokenType = 'word', n) {
      if (n == 1) {
        ngrams <- NLPStudio::tokenize(text, tokenType = 'word', ngrams = n)
      } else {
        ngrams <- unlist(lapply(seq_along(text), function(i) {
          NLPStudio::tokenize(text[i], tokenType = 'word', ngrams = n)
        }))
      }
      return(ngrams)
    },

    annotateText = function(n) {
      document <- private$..document$content
      text <- unlist(lapply(seq_along(document), function(i) {
        paste(paste0(rep("BOS", times = n-1), collapse = " "), document[i], "EOS", collapse = " ")
      }))
      return(text)
    },

    buildTables = function() {

      private$..nGrams <- lapply(seq(1:private$..size), function(n) {
        text <- private$annotateText(n)
        ngrams <- private$createNGrams(text, tokenType = 'word', n = n)
        private$createTable(ngrams, n)
      })

      names(private$..nGrams) <- private$..modelType[1:private$..size]

      return(TRUE)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('KN', 'Katz', 'SBO'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..lm <- x
      private$..document <- x$getDocument()
      private$..size <- x$getSize()
      invisible(self)
    },

    build = function() {

      # Build ngram tables with raw frequency counts
      private$buildTables()

      # Add context and continuation counts
      private$counts()

      # Compute discounts
      private$discounts()

      # Update language model
      private$..lm$setnGrams(private$..nGrams)
      private$..lm$setDiscounts(private$..discount)
      private$..lm$setTotals(private$..totals)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knCounts(self)
    }
  )
)
