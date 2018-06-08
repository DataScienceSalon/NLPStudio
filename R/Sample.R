#' Sample
#'
#' \code{Sample} Class representing sampled Document objects.
#'
#' Class containing the sampled representation of a Document object.
#'
#' @usage Sample <- Sample$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the Sample class.}
#'   \item{\code{content}}{Active binding used to set and retrieve Sample content. Sample
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   Sample content.}
#'  }
#'
#' @param x The source Document or Corpus object.
#' @param n Numeric. If n <= 1, the proportion of 'unit' to sample. If n>1, the number
#' of 'unit' to sample.
#' @param unit Character indicating the units to sample. Values
#' include c('word', 'sentence', 'vector'). The default is 'sentence'
#' @param stratify Logical. Applies to Corpus objects with with two or more documents.
#' If TRUE, stratified sampling will occur within each document; otherwise, samples
#' will be taken from a single combined document. Default is TRUE
#' @param replace Logical. If TRUE, samples will be taken with replacement. Default is FALSE.
#' @param seed Numeric. Seed for sampling reproducibility. Default is NULL
#'
#' @return Sample object, containing the Sample for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Sample <- R6::R6Class(
  classname = "Sample",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..n = numeric(),
    ..name = character(),
    ..stratify = logical(),
    ..replace = logical(),
    ..seed = numeric(),

    validate = function(x, n, unit, stratify, replace) {

      # Validate Source Object
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$logicals$variables <- c('stratify', 'replace')
      private$..params$logicals$values <- c(stratify, replace)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      if (class(x)[1] == 'Corpus' & (stratify)) {
        nDocuments <- x$getMeta(key = 'documents')
        if (length(n) != 1 & length(n) != nDocuments) {
          event <- paste0("Invalid n parameter. Must be of length one, or ",
                          "length ",nDocuments,", the number of documents in ",
                          "the corpus. See?", class(self)[1], " for further ",
                          "assistance.")
          private$logR$log(method = 'this', event = event, level = "Error")
          stop()
        }
      }
    },

    sampleDocument = function(x) {

      size <- private$..n
      if (size <= 1) size <- floor(size * length(x$content))

      if (length(private$..seed) > 0) set.seed(private$..seed)

      idx <- sample(1:length(x$content), size = size, replace = private$..replace)
      samples <- x$content[idx]

      # Create Document
      name <- paste0(x$getName(), " (sample)")
      doc <- Document$new(x = samples, name = name)
      return(doc)
    },

    sampleCorpus = function() {

      # Create corpus
      corpus <- Clone$new()$this(x = private$..x, reference = FALSE)
      if (is.null(private$..name)) private$..name <- paste0(private$..x$getName(), " (sample)")
      corpus$setName(private$..name)

      # Extract and if stratify is false, combine documents
      documents <- private$..x$getDocuments()
      if (!private$..stratify) {
        docText <- unlist(lapply(documents, function(d) { d$content }))
        documents <- list()
        documents[[1]] <- Document$new(x = docText, name = paste0("Corpus ",
                                                                  private$..x$getName(), " document"))
      }

      # Sample documents and add to corpus
      for (i in 1:length(documents)) {
        doc <- private$sampleDocument(documents[[i]])
        corpus$addDocument(doc)
      }

      return(corpus)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Constructor                                   #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadServices()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Sample                                    #
    #-------------------------------------------------------------------------#
    this = function(x, n, name = NULL, stratify = TRUE,
                    replace = FALSE, seed = NULL) {

      private$validate(x, n, unit, stratify, replace)
      private$..x <- Clone$new()$this(x, reference = TRUE)
      private$..n <- n
      private$..name <- name
      private$..stratify <- stratify
      private$..replace <- replace
      private$..seed <- seed


      if (class(x)[1] == 'Corpus') {
        sample <- private$sampleCorpus()
      } else {
        sample <- private$sampleDocument(private$..x)
      }

      event <- paste0("Sampled ", class(x)[1], ", ", x$getName(), ".")
      private$logR$log(method = 'this', event = event)
      return(sample)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$sample(self)
    }
  )
)
