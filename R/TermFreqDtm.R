#' TermFreqDtm
#'
#' \code{TermFreqDtm} Domain class containing a term frequency matrix produced by the \code{\link[NLPStudio]{TermFreqFactoryDtm}} class.
#'
#' TermFreqDtm objects are wrappers for the tm package DocumentTermMatrix class.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the TermFreqDtm class.}
#'   \item{\code{tdm()}}{Active binding that returns the DocumentTermMatrix object.
#'   object. The current DocumentTermMatrix is set through assignment.}
#'   }
#'
#' @param x A tm package DocumentTermMatrix object.
#' @template metadataParams
#'
#' @return The tdm method returns a DocumentTermMatrix object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreqDtm <- R6::R6Class(
  classname = "TermFreqDtm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..x = character(),
    ..tdm = character()
  ),

  active = list(
    dtm = function(value) {
      if (missing(value)) {
        private$..dtm
      } else {
        if (class(value)[1] %in% c("DocumentTermMatrix")) {
          private$..dtm <- value
        } else {
          event <- "Parameter is not a valid DocumentTermMatrix object."
          private$logR$log(cls = class(self)[1], event = event, "Error")
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
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

    get = function() { return(private$..dtm) },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqDtm(self)
    }
  )
)
