#' NGram
#'
#' \code{NGram} Class representing tokenized Document objects.
#'
#' Class containing the tokenized representation of a Document object.
#'
#' @usage tokens <- NGram$new()
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(x)}}{Initializes an object of the NGram class.}
#'   \item{\code{content}}{Active binding used to set and retrieve NGram content. NGram
#'   content may be changed via assignment. Referencing this method retrieves the current
#'   NGram content.}
#'  }
#'
#' @param x The source Document object.
#' @template metadataParams
#'
#' @return NGram object, containing the tokens for a single Document object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family NGram Classes
#' @export
NGram <- R6::R6Class(
  classname = "NGram",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..x = character(),
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

    getStats = function() {

      modified <- private$meta$get(key = 'modified')

      if (length(private$..timestamp) > 0) {
        if (private$..timestamp > modified) {
          return(TRUE)
        }
      }

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
    #                           Constructor                                   #
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
      private$meta <- Meta$new(x = self)
      private$logR$log(method = 'initialize', event = "Initialization complete.")
      invisible(self)
    },

    getNGrams = function() { return(private$..content) },

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
    getDocument = function() { return(private$..x) },
    getType = function() { return(private$meta$get(key = 'nGramType')) },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      sd <- list()
      sd$id <- private$summarizeIdMeta()
      sd$quant <- private$summarizeQuantMeta()
      sd$counts <- private$summarizeTopNGrams()
      sd$spectrum <- private$summarizeSpectrum()
      sd$functional  <- private$summarizeFunctionalMeta()
      sd$admin <- private$summarizeAdminMeta()
      sd$tech <- private$summarizeTechMeta()
      invisible(sd)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$nGram(self)
    }
  )
)
