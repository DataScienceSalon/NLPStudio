#' Tokenizer
#'
#' \code{Tokenizer} Creates a Tokenizer object from a Corpus.
#'
#' Class creates a character, word, sentence or paragraph Tokenizer object
#' from a Corpus object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Tokenizer class.}
#'   \item{\code{execute(x, train = 0.75, validation = 0, test = 0.25,
#'   stratify = TRUE, seed = NULL)}}{Executes the corpus Tokenizers.}
#'  }
#'
#' @param x Corpus object.
#' @param tokenUnit Character string indicating the token unit. Valid values
#' are 'character', 'word', 'sentence', and 'paragraph'. Abbreviations
#' are permitted.
#'
#' @return Tokenizer object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Tokenizer Family of Classes
#' @family CorpusStudio Family of Classes
#' @export
Tokenizer <- R6::R6Class(
  classname = "Tokenizer",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    validate = function(corpus, tokenUnit) {
      private$..params <- list()
      private$..params$classes$name <- list('corpus','tokenUnit')
      private$..params$classes$objects <- list(corpus, tokenUnit)
      private$..params$classes$valid <- list('Corpus', 'character')
      private$..params$discrete$variables = list('tokenUnit')
      private$..params$discrete$values = list(tokenUnit)
      private$..params$discrete$valid = list(c('character', 'word', 'sentence',
                                               'paragraph', 'c', 'w', 's', 'p'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        stop()
      }

      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Tokenizer Method                            #
    #-------------------------------------------------------------------------#
    execute = function(corpus, name = NULL, tokenUnit = 'sentence') {

      private$validate(corpus, tokenUnit)
      if (grepl("^c", tokenUnit, ignore.case = TRUE)) tokenUnit <- 'Character'
      if (grepl("^w", tokenUnit, ignore.case = TRUE)) tokenUnit <- 'Word'
      if (grepl("^s", tokenUnit, ignore.case = TRUE)) tokenUnit <- 'Sentence'
      if (grepl("^p", tokenUnit, ignore.case = TRUE)) tokenUnit <- 'Paragraph'

      token <- Corpus$new()
      token <- Copy$new()$this(x = corpus, to = token)
      token$setMeta(key = 'tokenUnit', value = tokenUnit, type = 'f')

      documents <- corpus$getDocuments()
      for (i in 1:length(documents)) {
        name <- documents[[i]]$getName()
        tokens <- unlist(NLPStudio::tokenize(documents[[i]]$content,
                                             tokenUnit = tokenUnit))
        document <- Document$new(x = tokens,
                                 name = paste0(name, "( ", tokenUnit, " Tokenizers )"))
        document$setMeta(key = 'tokenUnit', value = tokenUnit, type = 'f')
        token$addDocument(document)
      }
      return(token)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokenizer(self)
    }
  )
)
