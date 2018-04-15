#------------------------------------------------------------------------------#
#                            tm DocumentTermMatrix                             #
#------------------------------------------------------------------------------#
#' TermFreqFactoryDtm
#'
#' \code{TermFreqFactoryDtm}  Strategy for building term frequency matrix objects from tm package DocumentTermMatrix objects.
#'
#' A wrapper for tm package DocumentTermMatrix, this classes creates a sparse
#' document-term matrix for a Corpus object.
#' Source \url{https://cran.r-project.org/web/packages/tm/tm.pdf.}
#'
#' @usage TermFreqFactoryDtm$new(x, tolower = TRUE, stem = FALSE, dictionary = NULL)$execute()
#'
#' @param x A Corpus object
#' @param tolower if TRUE, converts all letters to lower case. Default is TRUE.
#' @param stem if TRUE, stem words. Default is FALSE.
#' @param dictionary A character vector to be tabulated against. No other
#' terms will be listed in the result. Defaults to NULL which means
#' that all terms in doc are listed.
#'
#' @return \code{\link{TermFreqFactoryDtm}} object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @export
TermFreqFactoryDtm <- R6::R6Class(
  classname = "TermFreqFactoryDtm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TermFreqFactory0,

  private = list(

    processCorpus = function() {

      corpus <- ConverterTM$new()$convert(private$..x)
      private$..termFreq$dtm <- tm::DocumentTermMatrix(corpus,
                                                   control = list(
                                                     tolower = private$..settings$tolower,
                                                     stemming = private$..settings$stem,
                                                     dictionary = private$..settings$dictionary)
                                                   )
      return(TRUE)
    }
  ),

  public = list(

    initialize = function(x, tolower = TRUE, stem = FALSE, dictionary = NULL) {

      private$loadDependencies()

      # Validate parameters
      private$..params$x <- x
      private$..params$logicals$variables <- c("tolower", "stem")
      private$..params$logicals$values <- c(tolower, stem)
      if (private$validate()$code == FALSE) stop()

      private$..x <- x
      private$..settings$tolower <- tolower
      private$..settings$stem <- stem
      private$..settings$dictionary <- dictionary
      name <- paste0(x$getName() ," Document Term Matrix")
      corpusId <- x$getId()

      private$..termFreq <- TermFreqDtm$new(name = name, corpusId = corpusId)

      private$logR$log(cls = class(self)[1], event = "Initialized.")
      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      private$processCorpus()

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      private$logR$log(cls = class(self)[1], event = event)

      return(private$..termFreq)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$termFreqFactoryDtm(self)
    }
  )
)
