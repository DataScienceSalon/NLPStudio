#------------------------------------------------------------------------------#
#                                     slice                                    #
#------------------------------------------------------------------------------#
#' slice
#'
#' \code{tokenize} Slices a vector into chunks of the given size
#' @param v Input vector to be split
#' @param size Numeric indicating the number of elements per slice.
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
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
#' \code{tokenize} Creates sentence, word and character tokens
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
tokenize = function(x, tokenType = 'char') {

  # Collapse into single vector
  x <- paste(x, collapse = " ")

  # Produce Tokenizer
  if (tokenType %in% c("sentence")) {

    # Use sentence token from openNLP and NLP packages
    s <- NLP::as.String(x)
    sa <- openNLP::Maxent_Sent_Token_Annotator()
    a <- NLP::annotate(s, sa)
    tokens <- s[a]
    nTokens <- length(tokens)

  } else {
    tokens <- quanteda::tokens(x = x, what = tokenType)$text1
  }
  return(tokens)
}

#------------------------------------------------------------------------------#
#                                Proper                                        #
#------------------------------------------------------------------------------#
#' proper
#'
#' \code{proper} Converts text string to proper case
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
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
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
#' @export
printHeading <- function(text, symbol = "=", newlines = 1) {
  leftPad <- 38 - floor(nchar(text)/2)
  rightPad <- 80 - leftPad - nchar(text) - 2
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
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Internal Functions
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
