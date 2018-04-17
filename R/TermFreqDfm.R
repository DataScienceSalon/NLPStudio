#' TermFreqDfm
#'
#' \code{TermFreqDfm} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqFactoryDfm}} class.
#'
#' TermFreqDfm objects are wrappers for the \code{\link[quanteda]{dfm}} class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqDfm class.}
#'   \item{\code{dfm(value)}}{Active binding that sets the value of the dfm object via
#'   assignment.  If no value is assigned, the method returns the current quanteda dfm object.}
#'   }
#'
#' @param x A quanteda dfm object.
#' @template metadataParams
#'
#' @return The dfm method returns a quanteda dfm object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqDfm <- R6::R6Class(
  classname = "TermFreqDfm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..x = character(),
    ..dfm = character()
  ),

  active = list(
    dfm = function(value) {
      if (missing(value)) {
        private$..dfm
      } else {
        if (quanteda::is.dfm(value)) {
          private$..dfm <- value
        } else {
          event <- "Parameter is not a valid dfm object."
          private$logR$log(cls = class(self)[1], event = event, "Error")
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Public Methods                              #
    #-------------------------------------------------------------------------#
    initialize = function(x, name = NULL, tolower = TRUE,
                          stem = FALSE, dictionary = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      private$..params$logicals$variables <- c("tolower", "stem")
      private$..params$logicals$values <- c(tolower, stem)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      self$setName(name = name)
      private$meta$set(key = 'tolower', value = tolower, type = 'f')
      private$meta$set(key = 'stem', value = stem, type = 'f')
      private$logR$log(method = 'initialize', event = "Initialized.")
      invisible(self)
    },

    get = function() { return(private$..dfm) },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqDfm(self)
    }
  )
)
