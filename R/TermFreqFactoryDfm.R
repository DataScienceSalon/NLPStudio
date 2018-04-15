#------------------------------------------------------------------------------#
#                      Quanteda (DFM) Term Frequency Matrix                    #
#------------------------------------------------------------------------------#
#' TermFreqFactoryDfm
#'
#' \code{TermFreqFactoryDfm}  Strategy for building term frequency matrix objects from quanteda dfm objects.
#'
#' A wrapper for \code{\link[quanteda]{dfm}}, this classes creates a sparse
#' document frequency matrix for a Corpus object.
#' Source \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#'
#' @usage TermFreqFactoryDfm$new(x, tolower = TRUE, stem = FALSE, dictionary = NULL)$execute()
#'
#' @param x A Corpus object
#' @param tolower if TRUE, converts all letters to lower case. Default is TRUE.
#' @param stem if TRUE, stem words. Default is FALSE.
#' @param dictionary a quanteda dictionary class object or a named list
#' of character vector dictionary entries to apply to the tokens when creating the dfm.
#' See \code{\link[quanteda]{dfm}} for details.
#' @param ... Other parameters passed to  \code{\link[quanteda]{dfm}}
#'
#' @return \code{\link{TermFreqFactoryDfm}} object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @export
TermFreqFactoryDfm <- R6::R6Class(
  classname = "TermFreqFactoryDfm",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TermFreqFactory0,

  private = list(

    processCorpus = function() {

      dict <- NULL
      if (!is.null(private$..settings$dictionary)) {
        if (quanteda::is.dictionary(private$..settings$dictionary)) {
          dict <- private$..settings$dictionary
        } else if (class(private$..settings$dictionary)[1] == 'list') {
          dict <- quanteda::dictionary(private$..settings$dictionary)
        } else {
          event <- paste0("Invalid dictionary class. Dictionary variable must ",
                          "be a quanteda dictionary object or a named list of ",
                          "character vector dictionary entries. See ?",
                          "quanteda::dictionary for further assistance.")
          private$logR$log(cls = class(self)[1], event = event, level = "Error")
          stop()
        }

      }

      q <- ConverterQuanteda$new()$convert(private$..x)
      private$..termFreq$dfm <- quanteda::as.dfm(quanteda::dfm(q,
                                          tolower = private$..settings$tolower,
                                          stem = private$..settings$stem,
                                          dictionary = dict))
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
      name <- paste0(x$getName() ," Term Frequency Matrix")
      corpusId <- x$getId()

      private$..termFreq <- TermFreqDfm$new(name = name, corpusId = corpusId)

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
      visitor$termFreqFactoryDfm(self)
    }
  )
)
