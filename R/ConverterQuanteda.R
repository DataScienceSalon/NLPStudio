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

      # Obtain corpus and document metadata
      corpusMeta <- x$getMeta()
      docMeta <- x$getDocMeta(classname = 'Document')

      # Obtain corpus text and text names
      text <- x$text()
      docs <- x$getDocument()
      docNames <- unlist(lapply(docs, function(d) {
        d$getName()
      }))

      # Create quanteda corpus object
      qCorpus <- quanteda::corpus(text, docnames = docNames)

      # Assign corpus level metadata from descriptive metadata
      corpusMeta <- corpusMeta$descriptive
      vars <- names(corpusMeta)
      for (i in 1:length(vars)) {
          metacorpus(qCorpus, vars[i]) <- corpusMeta[[i]]
      }

      # Assign docvars
      docVars <- as.data.frame(docMeta$Document$descriptive)
      if (nrow(docVars) > 0) {
        vars <- names(docVars)
        for (i in 1:length(vars)) {
          docvars(x = qCorpus, field = vars[i]) <- docVars[,i]
        }
      }

      # Assign metadoc variables
      metaDoc <- as.data.frame(docMeta$Document$functional)
      if (nrow(metaDoc) > 0) {
        vars <- names(metaDoc)
        for (i in 1:length(vars)) {
          metadoc(x = qCorpus, field = vars[i]) <- metaDoc[,i]
        }
      }

      return(qCorpus)

    },

    from = function(x) {

      # Create NLPStudio Corpus
      corpus <- Corpus$new()

      # Obtain and transfer corpus metadata
      descriptive <- metacorpus(x)[!names(metacorpus(x)) %in% c('source', 'created')]
      vars <- names(descriptive)
      corpus$setMeta(key = vars, value = descriptive, type = 'd')
      corpus$setSource(key = 'source', value = metacorpus(x)['source'][1])

      # Create Document Objects from quanteda corpus text and add to corpus
      docNames <- docnames(x)
      for (i in 1:length(x$documents$texts)) {
        doc <- Document$new(x = x$documents$texts[i], name = docNames[i])
        corpus$addDocument(doc)
      }

      # Add document descriptive metadata
      corpus$setDocMeta(docMeta = docvars(x), classname = 'Document',
                        type = 'd')

      # Add document functional metadata
      corpus$setDocMeta(docMeta = metadoc(x), classname = 'Document',
                        type = 'f')
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
      private$logR$log(method = 'initialize', event = event)

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
        private$logR$log(method = 'convert', event = event, level = "Error")
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
