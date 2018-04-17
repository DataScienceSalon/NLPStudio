#' Tokens
#'
#' \code{Tokens} Class representing tokenized Document objects.
#'
#' Class containing the tokenized representation of a Document object.
#'
#' @usage tokens <- Tokens$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the Tokens class.}
#'   \item{\code{content}}{Active binding used to set and retrieve Tokens content. Tokens
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   Tokens content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return Tokens object, containing the tokens for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Tokens Classes
#' @export
Tokens <- R6::R6Class(
  classname = "Tokens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(

    ..x = character(),

    tokenize = function(tokenType = 'word') {

      content <- private$..x$content

      # Produce tokens
      if (tokenType %in% c("sentence")) {

        # Use sentence token from openNLP and NLP packages
        s <- paste(content, collapse = "")
        s <- NLP::as.String(s)
        sa <- openNLP::Maxent_Sent_Token_Annotator()
        a <- NLP::annotate(s, sa)
        private$..content <- s[a]
        ntokens <- length(private$..content)

      } else {
        private$..content <- quanteda::tokens(x = content, what = tokenType)
        ntokens <- length(private$..content[[1]])
      }

      # Update text, compute statistics and update admin information
      private$meta$set(key = 'tokenType', value = tokenType)
      private$meta$set(key = 'ntokens', value = ntokens)

      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Document')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    getTokens = function() { return(private$..content) },
    getText = function() { return(private$..x$content) },

    #-------------------------------------------------------------------------#
    #                           Word Tokens                                   #
    #-------------------------------------------------------------------------#
    word = function() {

      private$tokenize(tokenType = 'word')
      event <- paste0("Created word token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Word Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'word', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Sentence Tokens                               #
    #-------------------------------------------------------------------------#
    sentence = function() {

      private$..type <- "sentence"
      private$tokenize(tokenType = 'sentence')
      event <- paste0("Created sentence token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Sentence Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'sentence', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Character Tokens                                  #
    #-------------------------------------------------------------------------#
    char = function(x) {

      private$tokenize(tokenType = 'character')
      event <- paste0("Created character token representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (Character Tokens)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'character', event = event)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tokens(self)
    }
  )
)
