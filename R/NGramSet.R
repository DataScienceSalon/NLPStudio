#' NGramSet
#'
#' \code{NGramSet} Class containing a collection of NGram objects
#'
#' Class contains NGram objects
#'
#' @usage skiReport <- NGramSet$new(name = "skiReport", purpose = 'Train')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the NGramSet class.}
#'   \item{\code{word()}}{Creates word tokens.}
#'   \item{\code{sentence()}}{Creates sentence tokens.}
#'   \item{\code{char()}}{Creates character tokens.}
#'   \item{\code{get()}}{Returns the tokens.}
#'   \item{\code{getText()}}{Returns the original text.}
#'  }
#'
#' @param name Character string containing the name for the NGramSet object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the NGramSets
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return NGramSet object, containing the NGramSet text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family NGram Classes
#' @export
NGramSet <- R6::R6Class(
  classname = "NGramSet",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  private = list(
    ..x = character(),
    ..content = character(),
    ..counts = data.frame(),
    ..spectrum = data.frame(),
    ..timestamp = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeTopNGrams = function(n = 10) {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())
      type <- self$getType()

      counts <- self$getCounts()
      counts <- counts %>% arrange(desc(Freq))
      n <- min(n, nrow(counts))
      top <- counts[1:n,]

      if (nrow(top) > 0) {
        NLPStudio::printHeading(text = paste0(id, " Top ", n, " ", type, "s" ), symbol = "-")
        print(top, row.names = FALSE)
        return(top)
      }
      return(NULL)
    },

    summarizeSpectrum = function(n = 10) {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())
      type <- self$getType()

      spectrum <- self$getSpectrum()
      spectrum <- spectrum %>% arrange(FreqClass)
      n <- min(n, nrow(spectrum))
      top <- spectrum[1:n,]

      if (nrow(top) > 0) {
        NLPStudio::printHeading(text = paste0(id, " Top ", n, " Frequency Classes" ), symbol = "-")
        print(top, row.names = FALSE)
        return(top)
      }
      return(NULL)
    },


    mergeNGrams = function() {
      private$..content <- character()
      for (i in 1:length(private$..documents)) {
        private$..content <- c(private$..content,
                               private$..documents[[i]]$getNGrams())
      }
      return(TRUE)
    },

    getStats = function() {

      modified <- private$meta$get(key = 'modified')

      # Return if statistics already computed since document update
      if (length(private$..timestamp) > 0) {
        if (private$..timestamp > modified) {
          return(TRUE)
        }
      }

      # Merge nGrams from individual nGram Documents into content variable
      private$mergeNGrams()

      # Compute nGram Frequency counts
      private$..counts <- as.data.frame(table(private$..content),
                                        stringsAsFactors = FALSE,
                                        row.names = NULL)
      type <- self$getType()
      names(private$..counts) <- c(type, 'Freq')

      # Compute nGram Frequency Spectrum
      private$..spectrum <- as.data.frame(table(private$..counts$Freq),
                                          stringsAsFactors = FALSE,
                                          row.names = NULL)
      names(private$..spectrum) <- c("FreqClass", "Freq")

      private$..spectrum$FreqClass <- as.numeric(private$..spectrum$FreqClass)

      # Note timestamp
      private$..timestamp <- Sys.Date()

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
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    getNGrams = function() {
      private$mergeNGrams()
      return(private$..content) },

    getCounts = function() {
      private$getStats()
      return(private$..counts)
    },

    getSpectrum = function() {
      private$getStats()
      return(private$..spectrum)
    },

    get = function() {
      private$getStats()
      result <- list()
      result$nGrams <- private$..content
      result$counts <- private$..counts
      result$spectrum <- private$..spectrum
      return(result)
    },

    getCorpus = function() { return(private$..x) },
    getType = function() { return(private$meta$get(key = 'nGramType')) },
    getDocNGramObjects = function() { private$..documents },


    #-------------------------------------------------------------------------#
    #                          Composite Management                           #
    #-------------------------------------------------------------------------#
    addNGram = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('NGram'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addNGram',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      sd <- list()
      sd$id <- private$summarizeIdMeta()
      sd$quant <- private$summarizeQuantMeta()
      sd$counts  <- private$summarizeTopNGrams()
      sd$spectrum  <- private$summarizeSpectrum()
      sd$documents <- private$summarizeDocMeta()
      sd$functional  <- private$summarizeFunctionalMeta()
      sd$admin <- private$summarizeAdminMeta()
      sd$tech <- private$summarizeTechMeta()
      invisible(sd)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$nGramSet(self)
    }
  )
)
