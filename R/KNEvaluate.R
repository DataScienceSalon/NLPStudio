#==============================================================================#
#                               KNEvaluate                                     #
#==============================================================================#
#' KNEvaluate
#'
#' \code{KNEvaluate} Evaluates a Kneser-Ney Language Model on Test Set
#'
#' Evaluates a Kneser-Ney Language Model on Test Set by computing
#' test set perplexity
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family LMStudio Classes
#' @export
KNEvaluate <- R6::R6Class(
  classname = "KNEvaluate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(
    ..testCorpus = character(),
    ..quant = list(
      nGrams = numeric(),
      exact = numeric(),
      backOff = numeric()
    ),

    annotate = function(text) {

    },

    markOOV = function() {
      # Obtain unigrams from model
      nGrams <- private$..lm$getNGrams()
      vocabulary <- nGrams[[1]]$nGram

      # Obtain test set text
      docs <- private$..testCorpus$getDocuments()
      text <- unlist(lapply(docs, function(d) { d$content }))
      tokens <- unlist(tokenizers::tokenize_words(x = text))
      oov <- unlist(tokens[!(tokens %fin% vocabulary)])
      closedText <-
        stringi::stri_replace_all_regex(str = text,
                                        pattern = paste0('\\b', oov, '\\b'),
                                        replacement = rep('UNK', length(oov)),
                                        vectorize_all = FALSE)
      return(closedText)
    },

    prepareTestData = function() {
      text <- private$markOOV()
      text <- private$annotate(text)
      return(TRUE)
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x, testCorpus) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('KN'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..lm <- x
      private$..testCorpus <- testCorpus

      event <-  paste0("Instantiated KNEvaluate ")
      private$logR$log(method = 'initialize', event = event, level = "Progress")
      invisible(self)
    },

    evaluate = function() {

      private$prepareTestData()
      private$scoreTestData()
      private$perplexity()

      private$..lm$setEvaluation(private$..nGrams)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knEvaluate(self)
    }
  )
)
