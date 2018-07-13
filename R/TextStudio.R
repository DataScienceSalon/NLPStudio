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
      punct = "(?![\'-])[[:punct:]]",
      hyphen = '[-]',
      apostrophe = '[\']',
      trailingApostrophe = '[\']\\B',
      numbers = "\\d+\\S*",
      symbols  = "(?![.?!'-])[[:punct:]]",
      twitter = '\\B[@#]\\w*[a-zA-Z]+\\w*',
      url = "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)",
      email = "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}",
      strayApostrophe = "\\s'\\B|\\B'\\s*",
      strayHyphen = "\\s*-\\B|\\B-\\s*",
      strayComma = "\\s,\\B|\\B,\\s*",
      singles = '\\b[b-hj-z]{1}\\b',
      backtick = list(
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

      content <- iconv(content, "UTF-8", "ASCII", sub = "")

      if (private$..config$..lowercase) {
        content <- tolower(content)
      }

      # Remove apostrophe, trailingApostrophe, numbers, twitter, url and email
      regex <- c()
      if (private$..config$..remove$apostrophe)
        regex <- c(regex, private$..regex$apostrophe)
      if (private$..config$..remove$trailingApostrophe)
        regex <- c(regex, private$..regex$trailingApostrophe)
      if (private$..config$..remove$twitter)
        regex <- c(regex, private$..regex$twitter)
      if (private$..config$..remove$url)
        regex <- c(regex, private$..regex$url)
      if (private$..config$..remove$email)
        regex <- c(regex, private$..regex$email)

      content <- gsub(paste(regex, collapse = '|'), "", content, perl = TRUE, ignore.case = TRUE)
      regex <- NULL

      # Remove sentences/vectors with profanity
      if (private$..config$..remove$profanitySentences) {
        if (length(private$..config$profanity) == 0) {
          profanity <- NLPLists::profanity[,1]
        } else {
          profanity <- private$..config$profanity
        }
        pattern <- paste0("\\b", paste0(profanity, collapse = "|"), "(?!\\w)", sep = "")
        content <- content[!grepl(pattern, content, perl = TRUE)]
      }

      # Remove words/vectors with profanity
      if (private$..config$..remove$profanityWords) {
        if (length(private$..config$profanity) == 0) {
          profanity <- NLPLists::profanity[,1]
        } else {
          profanity <- private$..config$profanity
        }
        content <- textclean::mgsub_fixed(content, pattern = profanity,
                                          replacement = "",
                                          leadspace = FALSE,
                                          trailspace = FALSE,
                                          fixed = TRUE,
                                          trim = FALSE,
                                          order.pattern = TRUE)
      }

      # Process Abbreviations
      if (private$..config$..replace$abbreviations) {
        if (length(private$..config$..abbreviations) == 0) {
          abbreviations <- NLPLists::internetAbbreviations
        } else {
          abbreviations <- private$..config$..abbreviations
        }
        pattern <- paste0("\\b",unlist(abbreviations[,1]),"\\b")
        replacement <- unlist(abbreviations[,2])
        content <- stringi::stri_replace_all_regex(content,
                                                   replacement = replacement,
                                                   pattern = pattern, mode = all,
                                                   vectorize_all = FALSE)
      }

      # Process Hyphens
      if (private$..config$..replace$hyphen) {
        content <- gsub(pattern = private$..regex$hyphen, replacement = " ",
                        content,  perl = TRUE)
      }

      # Process Internet Slang
      if (private$..config$..replace$slang) {
        if (length(private$..config$slang) == 0) {
          slang <- NLPLists::internetAbbreviations
        } else {
          slang <- private$..config$slang
        }
        pattern <- paste0("\\b",unlist(slang[,1]),"\\b")
        replacement <- unlist(slang[,2])
        content <- stringi::stri_replace_all_regex(content,
                                                   replacement = replacement,
                                                   pattern = pattern, mode = all,
                                                   vectorize_all = FALSE)
      }

      # Process Backtick
      if (private$..config$..replace$backtick) {
        pattern <- private$..regex$backtick$pattern
        replacement <- private$..regex$backtick$replacement
        content <- gsub(pattern = pattern, replacement = replacement,
                        content, perl = TRUE, ignore.case = TRUE)
      }

      # Process Contractions
      if (private$..config$..replace$contractions) {
        if (length(private$..config$..contractions) == 0) {
          contractions <- NLPLists::contractions
        } else {
          contractions <- private$..config$..contractions
        }
        pattern <- paste0("\\b",unlist(contractions[,1]),"\\b")
        replacement <- unlist(contractions[,2])
        content <- stringi::stri_replace_all_regex(content,
                                                   replacement = replacement,
                                                   pattern = pattern, mode = all,
                                                   vectorize_all = FALSE)
      }

      # Process CurlyQuotes
      if (private$..config$..replace$curlyQuotes) {
        content <- textclean::replace_curly_quote(x = content)
      }

      # Process Emoji
      if (private$..config$..replace$emoji) {
        content <- textclean::replace_emoji(x = content)
      }

      # Process Emoticon
      if (private$..config$..replace$emoticon) {
        content <- textclean::replace_emoticon(x = content)
      }

      # Process Kern
      if (private$..config$..replace$kern) {
        content <- textclean::replace_kern(x = content)
      }

      # Process Numbers
      if (private$..config$..replace$numbers) {
        content <- textclean::replace_number(x = content, num.paste = TRUE)
      }

      # Process Ordinal
      if (private$..config$..replace$ordinal) {
        content <- textclean::replace_ordinal(x = content)
      }

      # Process Symbol
      if (private$..config$..replace$symbols) {
        content <- textclean::replace_symbol(x = content)
      }

      # Process Word Elongation
      if (private$..config$..replace$wordElongation) {
        content <- textclean::replace_word_elongation(x = content)
      }

      # Process Comma Space
      if (private$..config$..add$commaSpace) {
        pattern <- private$..regex$commaSpace$pattern
        replacement <- private$..regex$commaSpace$replacement
        content <- gsub(pattern = pattern, replacement = replacement,
                        content, perl = TRUE, ignore.case = TRUE)
      }

      # Clean symbols, punctuation, numbers, stray apostrophe, commas, and extra whitespace.
      regex <- NULL
      if (private$..config$..remove$symbols)
        regex <- c(regex, private$..regex$symbols)
      if (private$..config$..remove$punct)
        regex <- c(regex, private$..regex$punct)
      if (private$..config$..remove$numbers)
        regex <- c(regex, private$..regex$numbers)

      regex <- c(regex, private$..regex$strayApostrophe,
                 private$..regex$strayComma,
                 private$..regex$strayHyphen)

      content <- gsub(paste0(regex, collapse = '|'), "", content, perl = TRUE, ignore.case = TRUE)
      content <- textclean::replace_white(x = content)

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
    getClean = function()  private$..corpus,
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$textStudio(self)
    }
  )
)
