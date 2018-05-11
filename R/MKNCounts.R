#==============================================================================#
#                               MKNCounts                                      #
#==============================================================================#
#' MKNCounts
#'
#' \code{MKNCounts} Prepares text data for downstream processing.
#'
#' Gathers text data, creates sentence tokens and, if open is TRUE, converts
#' hapax legomenon to unknown tokens.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNCounts <- R6::R6Class(
  classname = "MKNCounts",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = MKNStudio0,

  private = list(

    discounts = function() {
      private$..discounts <- data.table::rbindlist(lapply(seq_along(private$..nGrams), function(x) {

        # Obtain frequency spectrum
        spectrum <- as.data.frame(table(private$..nGrams[[x]]$count), stringsAsFactors = FALSE)
        names(spectrum) <- c('count', 'freq')
        spectrum$count <- as.numeric(spectrum$count)
        if (is.na(spectrum[4,]$freq))  {
          spectrum <- rbind(spectrum, data.frame(count = 4, freq = 0))
        }

        # Compute discounts
        d <- list()
        d$n <- x
        d$D <- spectrum$freq[1] / (spectrum$freq[1] + 2 * spectrum$freq[2])
        d$D1 <- 1 - (2 * d$D * spectrum$freq[2] / spectrum$freq[1])
        d$D2 <- 2 - (3 * d$D * spectrum$freq[3] / spectrum$freq[2])
        d$D3 <- 3 - (4 * d$D * spectrum$freq[4] / spectrum$freq[3])
        d
      }))
      return(TRUE)
    },

    nContexts = function() {

      for (n in 1:private$..size) {
        if (n > 1) {
          private$..nGrams[[n]][,.(nContexts = .N), by = context]
        }
      }
      return(TRUE)
    },

    nHistAll = function() {

      for (i in 1:private$..size) {
        if (i > 1) {
          private$..nGrams[[i]][,.(histTypes = sum(length(unique(history))))]
        }
      }
    },

    cKN = function() {

      for (n in 1:private$..size) {

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
        current <- current[, discountLevel := ifelse(cKN > 3, 3, cKN)]
        private$..nGrams[[n]] <- current
      }
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
      private$..params$classes$valid <- list(c('MKN', 'Katz', 'SBO'))
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
      private$cKN()
      private$nHistAll()
      private$nContexts()

      # Compute discounts
      private$discounts()

      private$..lm$setTables(private$..tables)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknCounts(self)
    }
  )
)
