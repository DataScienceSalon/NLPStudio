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
