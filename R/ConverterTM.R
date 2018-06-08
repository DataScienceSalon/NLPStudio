#==============================================================================#
#                                 ConverterTM                                  #
#==============================================================================#
#' ConverterTM
#'
#' \code{ConverterTM} Converts NLPStudio Corpus objects to and from tm corpus objects.
#'
#' Given an NLPStudio Corpus object, this object returns a tm SimpleCorpus object.
#' Alternatively, a SimpleCorpus, VCorpus, or PCorpus produces a NLPStudio
#' Corpus object. Note: Document level metadata is not maintained when
#' converting from NLPStudio Document objects to tm Corpus objects. Document
#' level metadata must be added separately to the tm Corpus object.
#'
#' @usage ConverterTM$new()$convert(x)
#'
#' @param x Object to be converted
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Converter Classes
#' @export
ConverterTM <- R6::R6Class(
  classname = "ConverterTM",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Converter0,

  private = list(

    to = function(x) {

      # Obtain metadata
      cMeta <- c(x$getMeta()$descriptive, x$getMeta()$functional)
      cMetaVars <- names(cMeta)
      docMeta <- x$getDocMeta(classname = 'Document')$Document

      # Obtain documents
      docs <- x$getDocuments()

      # Convert list of text to character vectors
      text = character()
      for (i in 1:length(docs)) {
        text[i] <- paste(docs[[i]]$content, collapse = ' ')
      }

      # Format dataframe
      id <- data.frame(doc_id = docMeta$identity$id)
      descriptive <- docMeta$descriptive
      text <- as.data.frame(text)
      if (length(descriptive) > 0) {
        docs <- cbind(id, descriptive, text)
      } else {
        docs <- cbind(id, text)
      }

      # Create tm corpus object
      tmCorpus <- tm::Corpus(tm::DataframeSource(docs))

      # Add corpus level metadata
      if (length(cMetaVars) > 0) {
        for (i in 1:length(cMetaVars)) {
          NLP::meta(tmCorpus, tag = cMetaVars[i], type = "corpus") <- cMeta[[i]]
        }
      }

      return(tmCorpus)
    },

    from = function(x) {

      # Obtain metadata and text
      cMeta <- NLP::meta(x, type = "corpus")
      dMeta <- NLP::meta(x, type = "indexed")
      text <- lapply(seq_along(x), function(d) {
        x[[d]]$content
      })
      docNames <- names(x)

      # Create Documents from text.
      docs <- lapply(seq_along(text), function(t) { Document$new(x = text[[t]], name = docNames[t]) })

      for (i in 1:length(docNames)) {
        docs[[i]]$setMeta(key = 'source', value = "tm VCorpus text.", type = 'f')
        varnames <- names(dMeta)
        for (j in 1:length(varnames)) {
          if (length(dMeta[i,j]) > 0) {
            docs[[i]]$setMeta(key = varnames[j], value = dMeta[i,j])
          }
        }
      }

      # Create corpus object and meta data
      corpus <- Corpus$new()
      keys <- names(cMeta)
      values <- cMeta
      corpus$setMeta(key = keys, value = values)

      # Add documents
      for (i in 1:length(docs)) {
        corpus$addDocument(docs[[i]])
      }
      return(corpus)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadServices()
      event <- paste0("Initiated.")
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
      } else if (class(x)[1] %in% c('VCorpus', 'SimpleCorpus', 'PCorpus')) {
        return(private$from(x))
      } else {
        event <- paste0("Invalid class.  The class operates on ",
                                  "'Corpus', 'VCorpus', 'PCorpus', and ",
                                  "'SimpleCorpus' objects only.  See ?",
                                  class(self)[1], " for further assistance.")
        private$logR$log(classname = class(self)[1], event = event, level = "Error")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                              Visitor Method                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$converterTM(self)
    }
  )
)
