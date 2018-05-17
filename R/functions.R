#------------------------------------------------------------------------------#
#                                     slice                                    #
#------------------------------------------------------------------------------#
#' slice
#'
#' \code{removeNonPrintable} Removes select non-printable characters from file
#' using binary facilities. Defaults to removing all but line and form feed
#' characters.
#' @param path Character string including path to file
#' @param codes Numeric indicating the decimal codes for the characters
#' to be removed.
#' @return content Character vector containing content with non-printable
#' characters removed.
#' @export
removeNonPrintable <- function(path, codes = nonPrintables) {

  # Validate path and file type
  if (!R.utils::isFile(path)) stop(paste("File", path, "does not exist."))
  if (tools::file_ext(path) != "txt") {
    stop("This method operates on '.txt' files only.")
  }

  ioBin <- IOBin$new()
  ioTxt <- IOText$new()

  content <- ioBin$read(path = path)
  for (i in 1:length(codes)) {
    content[content == as.raw(codes[i])] = as.raw(0x20)
  }

  # Save to temp file, then re-read
  d <- tempfile(fileext = '.txt')
  ioBin$write(path = d, content = content)
  content <- ioTxt$read(path = d)
  unlink(d)
  return(content)
}



#------------------------------------------------------------------------------#
#                                     slice                                    #
#------------------------------------------------------------------------------#
#' slice
#'
#' \code{tokenize} Slices a vector into chunks of the given size
#' @param v Input vector to be split
#' @param size Numeric indicating the number of elements per slice.
slice = function(v, size) {
    starts <- seq(1,length(v),size)
    tt <- lapply(starts, function(y) v[y:(y+(size-1))])
    lapply(tt, function(x) x[!is.na(x)])
}


#------------------------------------------------------------------------------#
#                                  tokenize                                    #
#------------------------------------------------------------------------------#
#' tokenize
#'
#' \code{tokenize} Creates sentence, word and character tokens. A wrapper for
#' \code{\link[quanteda]{tokens}}
#' Source \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#'
#' @param x Character vector containing texts
#' @param tokenType Character taking the following values c('word', 'sentence', 'character')
#' @param removeNumbers Logical. Tokens including only numbers are removed if TRUE
#' @param removeTwitter Logical. Removes Twitter characters if set to TRUE
#' @param removeSymbols Logical. If TRUE, remove all characters in the Unicode "Symbol" [S] class
#' @param removePunct Logical. If TRUE, remove all characters in the Unicode "Punctuation" [P] class
#' @param removeURL Logical. If TRUE, find and eliminate URLs beginning with http(s)
#' @param removeHyphens Logical. If TRUE, split words that are connected by hyphenation and hyphenation-like characters in between words
#' @export
tokenize = function(x, tokenType = 'word', ngrams =  1, removeNumbers = FALSE,
                    removeTwitter = FALSE, removeSymbols = FALSE,
                    removePunct = FALSE, removeURL = FALSE,
                    removeHyphens = FALSE, wordsOnly = FALSE,
                    concatenator = " ") {

  if (wordsOnly) {
    removeNumbers <- TRUE
    removeSymbols <- TRUE
    removeTwitter <- TRUE
    removeURL <- TRUE
    removePunct <- TRUE
  }

  # Collapse into single vector
  x <- paste(x, collapse = " ")

  # Produce Tokenizer
  if (tokenType %in% c("sentence")) {

    # Use sentence token from openNLP and NLP packages
    s <- NLP::as.String(x)
    sa <- openNLP::Maxent_Sent_Token_Annotator()
    a <- NLP::annotate(s, sa)
    tokens <- s[a]

  } else {
    tokens <- quanteda::tokens(x = x, what = tokenType, ngrams = ngrams,
                               remove_numbers = removeNumbers,
                               remove_punct = removePunct,
                               remove_symbols = removeSymbols,
                               remove_twitter = removeTwitter,
                               remove_hyphens = removeHyphens,
                               remove_url = removeURL,
                               concatenator = concatenator)$text1
  }

  return(tokens)
}

#------------------------------------------------------------------------------#
#                                Proper                                        #
#------------------------------------------------------------------------------#
#' proper
#'
#' \code{proper} Converts text string to proper case
proper <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#------------------------------------------------------------------------------#
#                               PrintHeadings                                  #
#------------------------------------------------------------------------------#
#' printHeading
#'
#' \code{printHeading} Prints a three line heading with text centered on 2nd line
#' @export
printHeading <- function(text, symbol = "=", newlines = 1) {
  leftPad <- max(38 - floor(nchar(text)/2), 0)
  rightPad <- max(80 - leftPad - nchar(text) - 2, 8, 0)
  cat(rep("\n", newlines))
  cat(paste0("#", strrep(symbol, 78), "#", collapse = ""))
  cat(paste0("\n#", strrep(" ", leftPad), text,
               strrep(" ", rightPad) , "#", collapse = ""))
  cat(paste0("\n#", strrep(symbol, 78), "#", collapse = ""),"\n")
}

#------------------------------------------------------------------------------#
#                                  listFiles                                   #
#------------------------------------------------------------------------------#
#' listFiles
#'
#' \code{listFiles} Returns the list of files associated with a directory or a glob
#' @export
listFiles = function(x) {

  if (isDirectory(x)) {
    paths <- list.files(x, full.names = TRUE)
  } else {
    glob <- basename(x)
    dir <- dirname(x)
    paths <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
  }
  return(paths)
}
