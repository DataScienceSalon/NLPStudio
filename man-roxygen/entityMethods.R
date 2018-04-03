#' @section Metadata Method:
#'  \itemize{
#'   \item{\code{meta(key = NULL, value = NULL)}}{Provides facility for
#'   managing an object's metadata represented as key/value pairs in
#'   a list format. Metadata may be indicated via single key and
#'   value character strings or by pairs of character vectors. If
#'   no parameters are passed to the method, then it returns the
#'   current metadata, as well as system generated application
#'   and system metadata, in data frame format. If a character
#'   string or vector is passed via the key parameter, the value
#'   or values associated with the keys are returned.}
#'   \item{\code{log(cls = class(self)[1], event = NULL)}}{Class for posting to and
#'   retrieving an object's log. If the event parameter is provided,
#'   the event is posted to the object's log and the log is returned
#'   invisibly.  If the event parameter is NULL, the existing log
#'   is returned.}
#' }
