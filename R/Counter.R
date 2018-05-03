#------------------------------------------------------------------------------#
#                      Quanteda (DFM) Term Frequency Matrix                    #
#------------------------------------------------------------------------------#
#' Counter
#'
#' \code{Counter}  Strategy for building term frequency matrix objects from quanteda dfm objects.
#'
#' A wrapper for \code{\link[quanteda]{dfm}}, this classes creates a sparse
#' document frequency matrix for a Corpus object.
#' Source \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#'
#' @usage Counter$new(x, tolower = TRUE, stem = FALSE, dictionary = NULL)$execute()
#'
#' @param x A Corpus object
#' @param tolower if TRUE, converts all letters to lower case. Default is TRUE.
#' @param stem if TRUE, stem words. Default is FALSE.
#' @param dictionary a quanteda dictionary class object or a named list
#' of character vector dictionary entries to apply to the tokens when creating the dfm.
#' See \code{\link[quanteda]{dfm}} for details.
#' @param ... Other parameters passed to  \code{\link[quanteda]{dfm}}
#'
#' @return \code{\link{Counter}} object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio Classes
#' @export
Counter <- R6::R6Class(
  classname = "Counter",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Counter0,

  private = list(

    tdm = function(x, tolower, stem, dictionary) {

      count <- Count$new(x, type = 'tdm')
      count <- Copy$new()$this(x, to = count)
      corpus <- ConverterTM$new()$convert(x)
      count$content <- tm::TermDocumentMatrix(corpus,
                                              control = list(
                                                tolower = tolower,
                                                stemming = stem,
                                                dictionary = dictionary)
      )
      return(count)
    },

    dtm = function(x, tolower, stem, dictionary) {

      count <- Count$new(x, type = 'dtm')
      count <- Copy$new()$this(x, to = count)
      corpus <- ConverterTM$new()$convert(x)
      count$content <- tm::DocumentTermMatrix(corpus,
                                              control = list(
                                                tolower = tolower,
                                                stemming = stem,
                                                dictionary = dictionary)
      )
      return(count)
    },

    dfm = function(x, tolower, stem, dictionary) {

      count <- Count$new(x, type = 'dfm')
      count <- Copy$new()$this(x, to = count)

      dict <- NULL
      if (!is.null(dictionary)) {
        if (quanteda::is.dictionary(dictionary)) {
          dict <- dictionary
        } else if (class(dictionary)[1] == 'list') {
          dict <- quanteda::dictionary(dictionary)
        } else {
          event <- paste0("Invalid dictionary class. Dictionary variable must ",
                          "be a quanteda dictionary object or a named list of ",
                          "character vector dictionary entries. See ?",
                          "quanteda::dictionary for further assistance.")
          private$logR$log(method = 'dfm', event = event, level = "Error")
          stop()
        }

      }

      q <- ConverterQuanteda$new()$convert(x)
      count$content <- quanteda::dfm(q, tolower = tolower, stem = stem,
                              dictionary = dict)
      return(count)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           DFM Creator                                   #
    #-------------------------------------------------------------------------#
    this = function(x, type , tolower = TRUE, stem = FALSE, dictionary = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      private$..params$logicals$variables <- c("tolower", "stem")
      private$..params$logicals$values <- c(tolower, stem)
      private$..params$discrete$variables = list('type')
      private$..params$discrete$values = list(type)
      private$..params$discrete$valid = list(c('tdm', 'dfm', 'dtm'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      count <- switch(type,
                      dtm = private$dtm(x, tolower, stem, dictionary),
                      tdm = private$tdm(x, tolower, stem, dictionary),
                      dfm = private$dfm(x, tolower, stem, dictionary))

      # Log it
      event <- paste0("Created a ", type, " object for ",
                      x$getName(), ". ")
      private$logR$log(method = 'this', event = event)

      return(count)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$counter(self)
    }
  )
)
