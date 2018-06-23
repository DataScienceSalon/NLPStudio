#------------------------------------------------------------------------------#
#                                    TextStudio                                #
#------------------------------------------------------------------------------#
#' TextStudio
#'
#' \code{TextStudio}  Class responsible for manipulating Corpus texts.
#'
#' Class contains methods for normalizing and cleaning Corpus object texts.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @export
TextStudio <- R6::R6Class(
  classname = "TextStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,
  private = list(
    ..corpus = character(),
    ..config = list(),
    ..regex = list(
      punct = "(?![-#%$&*+<=>@^_~|/\'])[[:punct:]]",
      hyphen = '[-]',
      apostrophe = '[\']',
      numbers = "(?<![a-zA-Z])(\\d+)(?![a-zA-Z])",
      symbols  = "[[:punct:]]",
      twitter = '\\B[@#]\\w*[a-zA-Z]+\\w*',
      url = "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)",
      email = "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}",
      strayApostrophe = "\\s*'\\B|\\B'\\s*",
      singles = '\\b[b-hj-z]{1}\\b',
      backTick = list(
        pattern = "\\`",
        replacement = "\\'"
      ),
      commaSpace = list(
        pattern = "(,)([^ ])",
        replacement = "\\1 \\2"
      )
    ),

    processDocument = function(document) {
      content <- document$content
      print(paste("initial length", length(content)))

      regex <- c()
      if (private$..config$..remove$punct)
        regex <- c(regex, private$..regex$punct)
      if (private$..config$..remove$numbers)
        regex <- c(regex, private$..regex$numbers)
      if (private$..config$..remove$symbols)
        regex <- c(regex, private$..regex$symbols)
      if (private$..config$..remove$twitter)
        regex <- c(regex, private$..regex$twitter)
      if (private$..config$..remove$url)
        regex <- c(regex, private$..regex$url)
      if (private$..config$..remove$email)
        regex <- c(regex, private$..regex$email)

      content <- gsub(paste(regex, collapse = '|'), "", content, perl = TRUE, ignore.case = TRUE)
      regex <- NULL

      print(paste("initial regex", length(content)))

      # Remove sentences/vectors with profanity
      if (private$..config$..remove$profanity) {
        if (length(private$..config$profanity) == 0) {
          profanity <- NLPLists::profanity[,1]
        } else {
          profanity <- private$..config$profanity
        }
        pattern <- paste("\\b", paste(profanity, collapse = "|"), "(?!\\w)", sep = "")
        content <- content[!grepl(pattern, content, perl = TRUE)]
      }

      print(paste("post profanity", length(content)))

      # Process Abbreviations
      if (private$..config$..replace$abbreviations) {
        if (length(private$..config$..abbreviations) == 0) {
          content <- qdap::replace_abbreviation(text.var = content,
                                                ignore.case = TRUE)
        } else {
          pattern <- "\\b"%s+%unlist(private$..config$..abbreviations[,1])%s+%"\\b"
          replacement <- unlist(private$..config$..abbreviations[,2])
          content <- stringi::stri_replace_all_regex(content,
                                                     replacement = replacement,
                                                     pattern = pattern, mode = all,
                                                     vectorize_all = FALSE)
        }
      }

      print(paste("post abbreviations", length(content)))

      # Process Hyphens
      if (private$..config$..replace$hyphen) {
        content <- gsub(pattern = private$..regex$hyphen, replacement = " ",
                        content,  perl = TRUE)
      }

      print(paste("post hyphens", length(content)))
      # Process Internet Slang
      if (private$..config$..replace$slang) {
        if (length(private$..config$slang) == 0) {
          slang <- NLPLists::internetAbbreviations
        } else {
          slang <- private$..config$slang
        }
        pattern <- "\\b"%s+%unlist(slang[,1])%s+%"\\b"
        replacement <- unlist(slang[,2])
        content <- stringi::stri_replace_all_regex(content,
                                                   replacement = replacement,
                                                   pattern = pattern, mode = all,
                                                   vectorize_all = FALSE)
      }
      print(paste("post slang", length(content)))

      # Process Backtick
      if (private$..config$..replace$backtick) {
        pattern <- private$..regex$backTick$pattern
        replacement <- private$..regex$backTick$replacement
        content <- gsub(pattern = pattern, replacement = replacement,
                        content, perl = TRUE, ignore.case = TRUE)
      }

      print(paste("post backtick", length(content)))

      # Process Contractions
      if (private$..config$..replace$contractions) {
        if (length(private$..config$..contractions) == 0) {
          contractions <- NLPLists::contractions
        } else {
          contractions <- private$..config$..contractions
        }
        pattern <- "\\b"%s+%unlist(contractions[,1])%s+%"\\b"
        replacement <- unlist(contractions[,2])
        content <- stringi::stri_replace_all_regex(content,
                                                   replacement = replacement,
                                                   pattern = pattern, mode = all,
                                                   vectorize_all = FALSE)
      }

      print(paste("post contractions", length(content)))

      # Process CurlyQuotes
      if (private$..config$..replace$curlyQuotes) {
        content <- textclean::replace_curly_quote(x = content)
      }

      print(paste("post curlyquotes", length(content)))
      print(head(content))

      # Process Emoji
      if (private$..config$..replace$emoji) {
        content <- textclean::replace_emoji(x = content)
      }

      print(paste("post emoji", length(content)))

      # Process Emoticon
      if (private$..config$..replace$emoticon) {
        content <- textclean::replace_emoticon(x = content)
      }

      print(paste("post emoticon", length(content)))

      # Process Kern
      if (private$..config$..replace$kern) {
        content <- textclean::replace_kern(x = content)
      }

      print(paste("post kern", length(content)))

      # Process Numbers
      if (private$..config$..replace$numbers) {
        content <- textclean::replace_number(x = content)
      }

      print(paste("post numbers", length(content)))

      # Process Ordinal
      if (private$..config$..replace$ordinal) {
        content <- textclean::replace_ordinal(x = content)
      }

      print(paste("post ordinal", length(content)))

      # Process Symbol
      if (private$..config$..replace$symbols) {
        content <- textclean::replace_symbol(x = content)
      }

      print(paste("post symbol", length(content)))

      # Process Word Elongation
      if (private$..config$..replace$wordElongation) {
        content <- textclean::replace_word_elongation(x = content)
      }

      print(paste("post elongation", length(content)))

      # Process Comma Space
      if (private$..config$..add$commaSpace) {
        pattern <- private$..regex$commaSpace$pattern
        replacement <- private$..regex$commaSpace$replacement
        content <- gsub(pattern = pattern, replacement = replacement,
                        content, perl = TRUE, ignore.case = TRUE)
      }

      print(paste("post comma space", length(content)))
      content <- textclean::replace_white(x = content)

      print(paste("post whitespace", length(content)))

      document$content <- content
      return(document)
    },

    processCorpus = function() {
      documents <- private$..corpus$getDocuments()
      for (i in 1:length(documents)) {
        document <- private$processDocument(documents[[i]])
        private$..corpus$addDocument(document)
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices(name = 'TextStudio')
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                         Load Configuation                               #
    #-------------------------------------------------------------------------#
    loadConfig = function(x) {
      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('TextConfig'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'config', event = v$msg, level = "Error")
        stop()
      }
      private$..config <- x$getConfig()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Execute Method                                #
    #-------------------------------------------------------------------------#
    execute = function(corpus, name = NULL) {
      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('corpus')
      private$..params$classes$objects <- list(corpus)
      private$..params$classes$valid <- list(c('Corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'execute', event = v$msg, level = "Error")
        stop()
      }
      if (length(private$..config) == 0) {
        event <- paste0("TextConfig object is missing. ")
        private$logR$log(method = 'execute', event = event, level = "Error")
      }
      private$..corpus <- Clone$new()$this(corpus, reference = TRUE, content = TRUE)
      if (!is.null(name)) private$..corpus$setName(name)
      private$processCorpus()
      invisible(self)
    },
    getCorpus = function()  private$..corpus,
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio(self)
    }
  )
)
