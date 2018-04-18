#' POSCollection
#'
#' \code{POSCollection} Class containing a collection of POS objects
#'
#' Class contains POS objects
#'
#' @usage pos <- POSCollection$new(x)
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the POSCollection class.}
#'   \item{\code{tag()}}{Creates POS tags}
#'   \item{\code{get()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the POSCollection object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the POSCollections
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return POSCollection object, containing the POSCollection text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family POS Classes
#' @export
POSCollection <- R6::R6Class(
  classname = "POSCollection",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  private = list(

    #-------------------------------------------------------------------------#
    #                           Tokenize Methods                              #
    #-------------------------------------------------------------------------#
    tagCorpus = function() {
      docs <- private$..x$getDocument()

      # Process Document corpus text combined into a single document.
      if (private$..collapse) {

        corpusText <- paste(docs, lapply(docs, function(d) {
          d$text()
        }) , collapse = " ")
        corpusDoc <- Document$new(x = corpusText, name = self$getName())
        posObject <- POS$new(corpusDoc)$tag()
        self$addDocument(posObject)

      # Process individual documents
      } else {
        for (i in 1:length(docs)) {
          posObject <- POS$new(docs[[i]])$tag()
          self$addDocument(posObject)
        }
      }
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function(x, collapse = FALSE) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Corpus')
      private$..params$logicals$variables <- c("collapse")
      private$..params$logicals$values <- c(collapse)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..collapse <- collapse
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Get Methods                                   #
    #-------------------------------------------------------------------------#
    getTags = function() {
      pos <- lapply(private$..documents, function(d) {
        d$getTags()
      })
      if (length(pos) == 1) pos <- pos[[1]]
      return(pos)
    },

    getDistribution = function() {
      pos <- lapply(private$..documents, function(d) {
        d$getDistribution()
      })
      if (length(pos) == 1) pos <- pos[[1]]
      return(pos)
    },

    #-------------------------------------------------------------------------#
    #                         Tag Collection                                  #
    #-------------------------------------------------------------------------#
    tag = function() {

      private$tagCorpus()
      event <- paste0("POS tagged ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (POS)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'tag', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Management                           #
    #-------------------------------------------------------------------------#
    addDocument = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('POS'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addDocument',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$posCollection(self)
    }
  )
)
