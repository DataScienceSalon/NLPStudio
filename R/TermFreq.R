#' TermFreq
#'
#' \code{TermFreq} Class responsible for producing term frequency matrices.
#'
#' Class receives a request for a quanteda dfm, tm TermDocumentMatrix, or a
#' tm DocumenTermMatrix, then dispatches the request to the appropriate
#' concrete factory for production.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x, type = c("dfm", "dtm", "tdm"), tolower = TRUE,
#'   stem = FALSE, dictionary = NULL)}}{Initializes an object of the TermFreq class.}
#'   \item{\code{execute()}}{Executes the build via the appropriate factory.}
#'   }
#'
#' @param x A Corpus object
#' @param type Character string indicating the type of term frequency matrix
#' to produce. Valid values are 'dfm' for the quanteda dfm object, 'dtm' for
#' the tm package DocumentTermMatrix, and 'tdm' for the tm package
#' TermDocumentMatrix.
#' @param tolower Logical, if TRUE, converts text to lower case prior to creating
#' the term frequency matrix.  The Default is TRUE.
#' @param stem Logical, if TRUE, conducts stemming prior to creating
#' the term frequency matrix.
#' @param dictionary For the dfm objects, this is a list of named character
#' vectors of words to be included in the term frequency matrix. For
#' the tdm and dtm objects, the dictionary is a vector of terms. The default
#' is NULL.
#' @template metadataParams
#'
#' @return A dfm, TermDocumentMatrix, or a DocumentTermMatrix as requested.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Term Frequency Classes
#' @export
TermFreq <- R6::R6Class(
  classname = "TermFreq",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,

  private = list(
    ..x = character(),
    ..settings = list(
      tolower = logical(),
      stem = logical(),
      dictionary = character(),
      type = character()
    )
  ),


  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(x, tolower = TRUE, stem = FALSE, dictionary = NULL,
                          type = "dfm") {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c("tolower", "stem")
      private$..params$logicals$values <- c(tolower, stem)
      private$..params$discrete$variables <- c("type")
      private$..params$discrete$values <- c(type)
      private$..params$discrete$valid <- list(c("dfm", "dtm", "tdm"))
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..settings$tolower <- tolower
      private$..settings$stem <- stem
      private$..settings$dictionary <- dictionary
      private$..settings$type <- type

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Factory Method                                #
    #-------------------------------------------------------------------------#
    execute = function() {

      if (private$..settings$type == "dfm") {
        termFreq <- TermFreqFactoryDfm$new(x = private$..x,
                                           tolower = private$..settings$tolower,
                                           stem = private$..settings$stem,
                                           dictionary = private$..settings$dictionary)$execute()
      } else if (private$..settings$type == "tdm") {
        termFreq <- TermFreqFactoryTdm$new(x = private$..x,
                                           tolower = private$..settings$tolower,
                                           stem = private$..settings$stem,
                                           dictionary = private$..settings$dictionary)$execute()
      } else {
        termFreq <- TermFreqFactoryDtm$new(x = private$..x,
                                           tolower = private$..settings$tolower,
                                           stem = private$..settings$stem,
                                           dictionary = private$..settings$dictionary)$execute()
      }
      return(termFreq)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreq(self)
    }
  )
)
