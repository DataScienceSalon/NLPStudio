#==============================================================================#
#                               SLMStudio0                                      #
#==============================================================================#
#' SLMStudio0
#'
#' \code{SLMStudio0} Abstract class for the Statistical Language Model Studio (SLMStudio) family of classes.
#'
#' This abstract class defines common members for the SLMStudio family of classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @family Statistical Language Model Studio Classes
#' @export
SLMStudio0 <- R6::R6Class(
  classname = "SLMStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  private = list(
    ..config = character(),
    ..corpora = character(),
    ..model = list(),
    ..regex = list(
      prefix = list(
        bigrams   = "^((\\S+\\s+){0}\\S+).*$",
        trigrams  = "^((\\S+\\s+){1}\\S+).*$",
        quadgrams = "^((\\S+\\s+){2}\\S+).*$",
        quintgrams = "^((\\S+\\s+){3}\\S+).*$"
      ),
      suffix = list(
        bigrams = "^.*\\s+((?:\\S+\\s+){0}\\S+)$",
        trigrams = "^.*\\s+((?:\\S+\\s+){1}\\S+)$",
        quadgrams = "^.*\\s+((?:\\S+\\s+){2}\\S+)$",
        quintgrams = "^.*\\s+((?:\\S+\\s+){3}\\S+)$"
      )
    )
  ),

  public = list(
    initialize = function(x) { stop("Not implemented for this abstract/interface class.") },
    getConfig = function() private$..config,
    getCorpora = function() private$..corpora,
    getModel = function() private$..model
  )
)
