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
#' @param tokenType Character string indicating type of tokens to create.
#' Valid values include 'word', and 'sentence'.
#' @param lowercase Logical. If true, text is changed to lowercase prior to tokenization.
#' @param nGrams Numeric indicator of length of nGrams to create.
#' @param removeNumeric Logical.  If TRUE, numbers are removed prior to tokenization. Default is FALSE.
#' @param removePunct Logical. If TRUE, remove all characters in the Unicode "Punctuation" [P] class. Default is FALSE.
#' @export
tokenize = function(x, tokenType = 'word', nGrams = NULL, lowercase = TRUE, removePunct = FALSE,
                    removeNumeric = FALSE) {

  if (!is.null(nGrams)) {
    tokens <- tokenizers::tokenize_ngrams(x, lowercase = lowercase, n = nGrams, n_min = nGrams)

  } else if (tokenType == 'word') {
    tokens <- tokenizers::tokenize_words(x = x, lowercase = lowercase,
                                         strip_punct = removePunct,
                                         strip_numeric = removeNumeric,
                                         simplify = FALSE)
  } else {
    tokens <- tokenizers::tokenize_sentences(x = x, lowercase = lowercase,
                                             strip_punct = removePunct,
                                             simplify = FALSE)
  }
  return(tokens)
}
