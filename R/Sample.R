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
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return Sample object, containing the Sample for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Sample Classes
#' @export
Sample <- R6::R6Class(
  classname = "Sample",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    validate = function(x, n, prob, unit, size, stratified, replace) {
      # Validate Source Object
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Document', 'Corpus'))
      private$..params$discrete$variables <- list(c('unit'))
      private$..params$discrete$values <- list(c(unit))
      private$..params$discrete$valid <- list(c('vector', 'sentence', 'word',
                                                'v', 's', 'w'))
      private$..params$logical$variables <- c('stratified', 'replace')
      private$..params$logical$values <- c(stratified, replace)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      if (!is.null(prob)) {
        if (sum(prob) == 0) {
          event <- paste0("Probabilities must not be all zero. ",
                          "See?", class(self)[1], " for further ",
                          "assistance.")
          private$logR$log(method = 'this', event = event, level = "Error")
          stop()
        }
      }

      if (grepl("^w", unit, ignore.case = TRUE)) {
        type <- 'word'
      } else if (grepl("^s", unit, ignore.case = TRUE)) {
        type <- 'sentence'
      } else if (grepl("^v", unit, ignore.case = TRUE)) {
        type <- 'vector'
      } else {
        event <- paste0("Invalid unit. Valid types are c('word', 'sentence',",
                        ", 'vector'). See?", class(self)[1], " for further ",
                        "assistance.")
        private$logR$log(method = 'this', event = event, level = "Error")
        stop()
      }

      if (is.null(prob) & is.null(n)) {
        event <- paste0("Either the 'n' parameter or the 'prob' parameter must ",
                        "be provided. See?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'this', event = event, level = "Error")
        stop()
      }

      if (!is.null(prob) & !is.null(n)) {
        event <- paste0("Probability and n (size) parameters were both provided. ",
                        "The n parameter will be ignored. See?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'this', event = event, level = "Warn")
      }

      if (class(x)[1] == 'Corpus' & (stratified)) {
        nDocuments <- x$getMeta(key = 'documents')
        if (length(n) != 1 & length(n) != nDocuments) {
          event <- paste0("Invalid n parameter. Must be of length one, or ",
                          "length ",nDocuments,", the number of documents in ",
                          "the corpus. See?", class(self)[1], " for further ",
                          "assistance.")
          private$logR$log(method = 'this', event = event, level = "Error")
          stop()
        }
      } else {
        if (!is.null(prob)) {
          if (length(prob) > 1) {
            event <- paste0("Probability vector should be of length one. Only the first ",
                            "value will be used. See?", class(self)[1], " for further ",
                            "assistance.")
            private$logR$log(method = 'this', event = event, level = "Warn")
          }
        } else if (!is.null(n)) {
          if (length(n) > 1) {
            event <- paste0("The 'n' vector should be of length one. Only the first ",
                            "value will be used. See?", class(self)[1], " for further ",
                            "assistance.")
            private$logR$log(method = 'this', event = event, level = "Warn")
          }
        }
      }
    },


    sampleDocument = function(x, n, unit, size, replace, seed) {

      # Format text into designated units
      tokens <- NLPStudio::tokenize(x = x$text, tokenType = unit)

      # Combine the units into segments of the designated size
      segments <- slice(tokens, size)

      # Create samples
      if (n <= 1) n <- floor(n * length(segments))
      idx <- sample(1:length(segments), size = n, replace = replace)
      samples <- segments[idx]

      # Create Document
      name <- paste0(x$getName(), " (sample)")
      doc <- Document$new(x = samples, name = name)

      return(doc)
    },

    sampleCorpus = function(x, n, unit, size, name, stratified,
                            replace, seed) {

      # Create corpus
      corpus <- Clone$new()$this(x = x, reference = FALSE)
      if (is.null(name)) name <- paste0(x$getName(), " (sample)")
      corpus$setName(name)

      # Extract and if stratified is false, combine documents
      documents <- x$getDocuments()
      if (!stratified) {
        docText <- paste(unlist(lapply(documents, function(d) { d$text })), collapse = '')
        documents <- list()
        documents[[1]] <- Document$new(x = docText, name = paste0("Corpus ", x$getName, " document"))
      }

      # Format length one n / prob vectors
      if ((by == 'n') & length(n) == 1) {
        n = rep(n, length(documents))
      }

      # Sample documents and add to corpus
      for (i in 1:length(documents)) {
        doc <- private$sampleDocument(documents[[i]], n[i], unit, size,
                                      replace, seed)
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

      private$loadDependencies()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Sample                                    #
    #-------------------------------------------------------------------------#
    this = function(x, n, unit = 'word', size, name = NULL, stratified = TRUE,
                    replace = FALSE, seed = NULL) {

      private$validate(x, n, unit, size, stratified, replace)


      if (class(x)[1] == 'Corpus') {
        sample <- private$sampleCorpus(x, n, unit, size, name, stratified,
                                       replace, seed)
      } else {
        sample <- private$sampleDocument(x, n, unit, size, replace, seed)
      }

      event <- paste0("Sampled ", class(x)[1], ", ", x$getName(), ".")
      private$logR$log(method = 'this', event = event)
      return(sample)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$sample(self)
    }
  )
)
