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
