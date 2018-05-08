#==============================================================================#
#                               MKNData                                        #
#==============================================================================#
#' MKNData
#'
#' \code{MKNData} Prepares text data for downstream processing.
#'
#' Gathers text data, creates sentence tokens and, if open is TRUE, converts
#' hapax legomenon to unknown tokens.
#'
#' @param lm Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNData <- R6::R6Class(
  classname = "MKNData",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = MKNStudio0,

  private = list(

    createTable = function(ngrams, n) {
      ngrams <- as.data.frame(table(ngrams), stringsAsFactors = FALSE)
      dt <- data.table(nGram = ngrams[,1],
                       rawCount = ngrams[,2])
      if (n > 1) {
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
      document <- private$..lm$getDocument()$content
      text <- unlist(lapply(seq_along(document), function(i) {
        paste(paste0(rep("BOS", times = n-1), collapse = " "), document[i], "EOS", collapse = " ")
      }))
      return(text)
    },

    buildTables = function(size) {

      private$..tables <- lapply(seq(1:size), function(n) {
        text <- private$annotateText(n)
        ngrams <- private$createNGrams(text, tokenType = 'word', ngrams = n)
        private$createTable(ngrams, n)
      })
      names(private$..tables) <- private$..modelType[1:size]
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
      private$..lm <- x
      invisible(self)
    },

    build = function() {

      size <- private$..lm$getSize()
      private$buildTables(size)
      private$count(size)
      nextStage <- MKNDiscounts$new(x = private$..lm)

      return(nextStage)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknCounts(self)
    }
  )
)
