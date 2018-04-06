#------------------------------------------------------------------------------#
#                            ConverterQuanteda                                 #
#------------------------------------------------------------------------------#
#' ConverterQuanteda
#'
#' \code{ConverterQuanteda} Converts NLPStudio Corpus objects to and from Quanteda corpus objects.
#'
#' @usage ConverterQuanteda$new()$convert(x)
#'
#' @param x Object to be converted
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter Classes
#' @export
ConverterQuanteda <- R6::R6Class(
  classname = "ConverterQuanteda",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Converter0,

  private = list(

    to = function(x) {

      # Extract metadata
      meta <- x$metadata()
      dMeta <- x$docMeta()

      # Create named corpus vectors, one document per vector
      docs <- x$getDocuments(cls = "TextDocument")
      content <- unlist(lapply(docs, function(d) {
        paste(d$content, collapse = "")
      }))
      names(content) <- dMeta$name

      # Create quanteda corpus object
      qCorpus <- quanteda::corpus(content, docnames = dMeta$name,
                                 docvars = dMeta, metacorpus = cMeta)
      return(qCorpus)
    },

    from = function(x) {

      # Obtain metadata
      cMeta <- quanteda::metacorpus(x)
      dMeta <- as.data.frame(quanteda::docvars(x))

      # Create corpus object and meta data
      corpus <- Corpus$new()
      keys <- names(cMeta)
      values <- cMeta
      corpus <- corpus$metadata(key = keys, value = values)

      # Add documents
      lapply(seq_along(x$documents$texts), function(t) {
        d <- TextDocument$new(name = as.character(x$documents$name[t]),
                          x = as.character(x$documents$texts[t]))
        corpus$addDocument(d)
      })

      # Add document metadata
      keys <- names(dMeta)
      for (i in 1:ncol(dMeta)) {
        corpus$docMeta(key = keys[i], value = dMeta[,i])
      }
      return(corpus)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadDependencies()

      event <- paste0("Initiated ", private$..classname)
      private$logR$log(cls = class(self)[1], event = event)

      invisible(self)


    },
    #-------------------------------------------------------------------------#
    #                           Conversion Methods                            #
    #-------------------------------------------------------------------------#
    convert = function(x) {

      private$..methodName <- 'convert'

      if (class(x)[1] == "Corpus") {
        return(private$to(x))
      } else if (class(x)[1] == "corpus") {
        return(private$from(x))
      } else {
        event <- paste0("This class operates on Corpus and quanteda ",
                                  "corpus objects only.")
        private$logR$log(cls = class(self)[1], event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                              Visitor Method                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$converterQuanteda(self)
    }
  )
)
