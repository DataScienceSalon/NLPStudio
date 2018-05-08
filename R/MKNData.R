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
#' @param x Language model object
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

    convert = function(corpus) {

      # Extract and collapse corpus text
      documents <- corpus$getDocuments()
      text <- paste(unlist(lapply(documents, function(d) {
        d$text})), collapse = " ")

      # Create new Document object and copy metadata from the Corpus object
      document <- Document$new(x = text)
      document <- Copy$new()$this(x = corpus, to = document)

      # Set metadata
      name <- private$..lm$getName()
      name <- paste0(name, " Document")
      document$setName(name)
      document$setMeta(key = 'smoothing', value = "Modified Kneser-Ney",
                        type = 'f')
      document$setMeta(key = 'modelType',
                       value = private$..lm$getSize(),
                       type = 'f')
      document$setMeta(key = 'openVocabulary',
                       value = private$..lm$is.openVocabulary(),
                       type = 'f')
      document$setMeta(key = 'lm',
                       value = 'Modified Kneser-Ney',
                       type = 'f')

      return(document)
    },

    processOOV = function(document) {

      nGrams <- NGrammer$new()$this(document, n = 1, wordsOnly = TRUE)
      counts <- nGrams$getCounts()
      hapax  <- (counts %>% filter(Freq == 1) %>% select(Unigram))$Unigram
      document$text <- textclean::replace_tokens(x = document$text, tokens = hapax,
                                   replacement = 'UNK')
      return(document)
    },


    sentencify = function(document) {
      tokenize <- Tokenizer$new()
      document <- tokenize$this(x = document, type = 's')
      return(document)
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

      corpus <- private$..lm$getCorpus()

      document <- private$convert(corpus)

      if (private$..lm$is.openVocabulary()) {
        corpus <- private$processOOV(document)
      }

      document <- private$sentencify(document)

      private$..lm$setDocument(document)

      #nextStage <- MKNCounts$new(x = private$..lm)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknData(self)
    }
  )
)
