#------------------------------------------------------------------------------#
#                               CorpusStudio                                   #
#------------------------------------------------------------------------------#
#' CorpusStudio
#'
#' \code{CorpusStudio}  Class responsible for building and manipulating Corpus objects.
#'
#' @param x a FileSet object containing .txt files or a character string
#' containing the directory containing .txt files.
#' @param corpus Corpus object
#' @param name Character string containing the name to assign to the Corpus
#' object.
#' @param path Character string containing a relative path to a directory
#' containing text files or a relative file path for a Corpus object. This
#' parameter is used in the load and save methods.
#' @param k Numeric used by the kFold method. Indicates number of folds in
#' which to split the Corpus object.
#' @param n Numeric parameter used by the sample method. It contains the number
#' of samples to obtain or the proportion of the Corpus to sample.
#' @param stratify Logical. If TRUE (default), splits and sampling will
#' be stratefied.
#' @param replace Logical. If TRUE, sampling is conducted with replacement. The
#' default is FALSE.
#' object.
#' @param train Numeric indicating the proportion of the Corpus to allocate
#' to the training set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param validation Numeric indicating the proportion of the Corpus to allocate
#' to the validation set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param test Numeric indicating the proportion of the Corpus to allocate
#' to the test set. Acceptable values are between 0 and 1. The total of the
#' values for the train, validation and test parameters must equal 1.
#' @param seed Numeric used to initialize a pseudorandom number generator.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family CorpusStudio Family of Classes
#' @export
CorpusStudio <- R6::R6Class(
  classname = "CorpusStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    #-------------------------------------------------------------------------#
    #                          Get File Paths Method                          #
    #-------------------------------------------------------------------------#
    getFilePaths = function(x) {

      if ("FileSet" %in% class(x)) {
        files <- x$getFiles()
        filePaths <- unlist(lapply(files, function(f) { f$getFilePath() }))
      } else if (isDirectory(x)) {
        filePaths <- list.files(x, full.names = TRUE)
      } else {
        glob <- basename(x)
        dir <- dirname(x)
        filePaths <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }
      return(filePaths)
    },
    #-------------------------------------------------------------------------#
    #                         Quant Summation Method                          #
    #-------------------------------------------------------------------------#
    sumQuant = function(corpus) {
      # Update quantitative metadata
      quant <- corpus$getDocMeta(type = 'q')[[1]]
      if (nrow(quant) > 0) {
        keys <- c(names(quant), 'documents')
        values <- c(colSums(quant), nrow(quant))
        corpus$setMeta(key = keys, value = values, type = 'q')
      }
      return(corpus)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(x, name = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('FileSet', 'character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build', event = v$msg, level = "Error")
        stop()
      }

      # Ascertain corpus source
      if ("FileSet" %in% class(x)) {
        corpusSource <- paste0(class(x)[1], " (", x$getName(), ")")
      } else {
        corpusSource <- x
      }

      # Instantiate Corpus
      corpus <- Corpus$new(name = name)
      corpus$setMeta(key = 'source', value = corpusSource, type = 'f')

      # Obtain file paths and create Document objects.
      filePaths <- private$getFilePaths(x)
      lapply(filePaths, function(p) {

        # Obtain content and extract document name from filepath
        io <- IOFactory$new()$strategy(p)
        content <- io$read(p)
        name <- tools::file_path_sans_ext(basename(p))

        # Create document object and update metadata
        doc <- Document$new(x = content, name = name)
        doc$setMeta(key = 'source', value = p, type = 'f')

        # Add Document object to Corpus
        corpus$addDocument(x = doc)
      })

      # Summarize quantitative data
      corpus <- private$sumQuant(corpus)

      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                       Corpus Reshape Method                             #
    #-------------------------------------------------------------------------#
    tokenize = function(corpus, tokenUnit = 'sentence') {
      documents <- corpus$getDocuments()
      lapply(documents, function(d) {
        d$content <- NLPStudio::tokenize(d$content, tokenUnit = tokenUnit)
        d$setMeta(key = 'tokenized', value = tokenUnit, type = 'f')
        corpus$addDocument(d)
      })
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sample = function(corpus, n, name = NULL, stratify = TRUE, replace = FALSE,
                      seed = NULL) {
      sampler <- Sample$new()
      corpus <- sampler$this(x = corpus, n = n, name = name, stratify = stratify,
                             replace = replace, seed = seed)
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    split = function(corpus, name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL) {
      splitter <- Split$new()
      cvSet <- splitter$execute(corpus, name, train, validation, test,
                                stratify, seed)
      return(cvSet)
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFold = function(corpus, k = 10, name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL)  {},

    #-------------------------------------------------------------------------#
    #                         Corpus Load Method                              #
    #-------------------------------------------------------------------------#
    load = function(path) {},

    #-------------------------------------------------------------------------#
    #                         Corpus Save Method                              #
    #-------------------------------------------------------------------------#
    save = function(path) {},

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
