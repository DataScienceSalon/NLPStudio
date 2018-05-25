#==============================================================================#
#                               KNDocument                                    #
#==============================================================================#
#' KNDocument
#'
#' \code{KNDocument} Prepares text data for downstream processing.
#'
#' Gathers text data, creates sentence tokens and, if open is TRUE, converts
#' hapax legomenon to unknown tokens.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family LMStudio Classes
#' @export
KNDocument <- R6::R6Class(
  classname = "KNDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(
    ..corpus = character(),

    convert = function() {
      # Converts a corpus with multiple documents into a corpus with a
      # single document.

      # Extract and collapse private$..corpus text
      documents <- private$..corpus$getDocuments()
      text <- paste(unlist(lapply(documents, function(d) {
        d$content})), collapse = " ")

      # Create new Document object and copy metadata from the private$..corpus object
      private$..document <- Document$new(x = text)
      private$..document <- Copy$new()$this(x = private$..corpus, to = private$..document)

      # Set metadata
      name <- private$..lm$getName()
      name <- paste0(name, " Document")
      private$..document$setName(name)
      private$..document$setMeta(key = 'smoothing', value = private$..smoothing,
                        type = 'f')
      private$..document$setMeta(key = 'modelType',
                       value = private$..size,
                       type = 'f')
      private$..document$setMeta(key = 'openVocabulary',
                       value = private$..lm$is.openVocabulary(),
                       type = 'f')
      private$..document$setMeta(key = 'lm',
                       value = private$..smoothing,
                       type = 'f')

      return(TRUE)
    },

    processOOV = function() {

      tokens <- NLPStudio::tokenize(x = private$..document$content, tokenType = 'word')
      freq <- as.data.frame(table(tokens), stringsAsFactors = FALSE)
      hapax  <- (freq %>% filter(Freq == 1) %>% select(tokens))
      private$..document$content <-
        textclean::replace_tokens(x = private$..document$content,
                                  tokens = hapax,
                                  replacement = 'UNK', ignore.case = TRUE)
      return(TRUE)
    },

    sentencify = function() {
      private$..document$content <- NLPStudio::tokenize(x = private$..document$content,
                                                     tokenType = 'sentence')
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

      # Dock language model (extract members read/modified in class)
      private$..lm <- x
      private$..smoothing <- 'Kneser-Ney'
      private$..open <- x$is.openVocabulary()
      private$..corpus <- x$getCorpus()
      private$..size <- x$getSize()

      print(paste0("Instantiated KNDocument at ", Sys.time()))
      invisible(self)
    },

    build = function() {

      private$convert()

      if (private$..open)  private$processOOV()

      private$sentencify()

      private$..lm$setDocument(private$..document)

      #nextStage <- KNCounts$new(x = private$..lm)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knDocument(self)
    }
  )
)
