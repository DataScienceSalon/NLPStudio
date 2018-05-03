#' Tagger
#'
#' \code{Tagger} Class containing a collection of POS objects
#'
#' Class contains POS objects
#'
#' @usage pos <- Tagger$new(x)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the Tagger class.}
#'   \item{\code{tag()}}{Creates POS tags}
#'   \item{\code{get()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the Tagger object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the Taggers
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return Tagger object, containing the Tagger text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family POS Classes
#' @export
Tagger <- R6::R6Class(
  classname = "Tagger",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  private = list(

    #-------------------------------------------------------------------------#
    #                           POS Tag Methods                               #
    #-------------------------------------------------------------------------#

    tagDocument = function(x) {

      tags <- POS$new(x)
      tags <- Copy$new()$this(x, to = tags)
      name <- paste0(x$getName(), " (POS)")
      tags$setName(name)

      pos <- list()
      content <- x$text
      s <- paste(content, collapse = "")
      s <- NLP::as.String(s)
      pa <- openNLP::Maxent_POS_Tag_Annotator()
      sa <- openNLP::Maxent_Sent_Token_Annotator()
      wa <- openNLP::Maxent_Word_Token_Annotator()
      a2 <- NLP::annotate(s, list(sa, wa))
      a3 <- NLP::annotate(s, pa, a2)
      a3w <- subset(a3, type == "word")
      pos$tags <- sapply(a3w$features, `[[`, "POS")
      pos$distribution <- table(pos$tags)
      tags$content <- pos

      return(tags)
    },

    tagCorpus = function(x) {

      posSet <- POSSet$new(x)
      posSet <- Copy$new()$this(x, to = posSet)
      name <- paste0(x$getName(), " (POS)")
      posSet$setName(name)

      docs <- x$getDocuments()
      for (i in 1:length(docs)) {
        tags <- private$tagDocument(docs[[i]])
        posSet$addTags(tags)
      }
      return(posSet)
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
    #                                 Tag Set                                 #
    #-------------------------------------------------------------------------#
    this = function(x) {

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus', 'Document'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'tag',
                         event = v$msg, level = "Error")
        stop()
      }

      if (class(x)[1] == 'Corpus') {
        tags <- private$tagCorpus(x)
      } else {
        tags <- private$tagDocument(x)
      }

      event <- paste0("POS tagged ", x$getName(), ".")
      private$logR$log(method = 'this', event = event)

      invisible(tags)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$tagger(self)
    }
  )
)
