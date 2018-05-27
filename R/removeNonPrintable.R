#------------------------------------------------------------------------------#
#                             Remove NonPrintable                              #
#------------------------------------------------------------------------------#
#' Remove NonPrintable
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
