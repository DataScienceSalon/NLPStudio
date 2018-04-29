#' POS
#'
#' \code{POS} Creates part-of-speech tagged documents.
#'
#' Class containing the methods for creating and reporting part-of-speech tags
#' for a Document object.
#'
#' @usage pos <- POS$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the POS class.}
#'   \item{\code{content}}{Active binding used to set and retrieve POS content. POS
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   POS content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return POS object, containing the tokens for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family POS Classes
#' @export
POS <- R6::R6Class(
  classname = "POS",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(

    ..x = character(),
    ..pos = list(),

    tagDocument = function() {

      content <- private$..x$content
      s <- paste(content, collapse = "")
      s <- NLP::as.String(s)
      pa <- openNLP::Maxent_POS_Tag_Annotator()
      sa <- openNLP::Maxent_Sent_Token_Annotator()
      wa <- openNLP::Maxent_Word_Token_Annotator()
      a2 <- NLP::annotate(s, list(sa, wa))
      a3 <- NLP::annotate(s, pa, a2)
      a3w <- subset(a3, type == "word")
      private$..pos$tags <- sapply(a3w$features, `[[`, "POS")
      private$..pos$distribution <- table(private$..pos$tag)

      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validate Source Document
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('Document')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      name = paste(x$getName(), "POS Tags")
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    get = function() { return(private$..pos) },
    getTags = function() { return(private$..pos$tags) },
    getDistribution = function() {return(private$..pos$distribution) },
    getText = function() { return(private$..x$content) },

    #-------------------------------------------------------------------------#
    #                            Render Tags                                  #
    #-------------------------------------------------------------------------#
    tag = function() {

      private$tagDocument()
      event <- paste0("Created POS tag representation of ",
                      private$..x$getName(), ".")
      name <- paste0(private$..x$getName(), " (POS)")
      self$setName(name = name)
      private$meta$modified(event = event)
      private$logR$log(method = 'tag', event = event)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$pos(self)
    }
  )
)
