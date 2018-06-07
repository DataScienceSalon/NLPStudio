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
#' @param mode Character string indicating the mode for writing a Corpus object.
#' Valid values are c("text", "binary", "object"). Abbreviations are permitted.
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

      sourceType <- class(x)[1]
      supportedClasses <- c('character', 'FileSet', 'corpus','VCorpus', 'SimpleCorpus')

      if (!(sourceType %in% supportedClasses)) {
        event <- paste0("Objects of class ", sourceType, " are not supported. ",
                        "See ?", class(self)[1], " for further assistance.")
        private$logR$log(method = 'build', event = event, level = "Error")
        stop()
      }

      if (sourceType == 'character') {
        if (isDirectory(x) | ((length(x) == 1) & sum(!grepl(" ", x, perl = TRUE)))) {
              sourceType <- 'directory'
            }
      }

      corpus <- switch(sourceType,
                       FileSet = CSourceFileSet$new()$source(x, name),
                       corpus = CSourceQuanteda$new()$source(x, name),
                       VCorpus = CSourceTM$new()$source(x, name),
                       SimpleCorpus = CSourceTM$new()$source(x, name),
                       character = CSourceVector$new()$source(x, name),
                       directory = CSourceDir$new()$source(x, name))
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                        Add Documents Method                             #
    #-------------------------------------------------------------------------#
    addDocuments = function(corpus, path) {
      if (!file.exists(path)) {
        filePaths <- NLPStudio:::listFiles(path)
        for (i in 1:length(filePaths)) {
          document <- Document$new(filePaths[i])
          corpus$addDocument(document)
        }
      }
      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Tokenize Method                             #
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
                     test = 0.25,  stratify = TRUE, seed = NULL)  {
      splitter <- SplitKFold$new()
      cvKFold <- splitter$execute(corpus, k, name, train, validation,
                                  test,  stratify, seed)
      return(cvKFold)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Write Method                              #
    #-------------------------------------------------------------------------#
    write = function(corpus, path, mode = "text") {
      mode <- tolower(substr(mode, 1, 1))
      if (!(mode %in% c('t', 'b', 'o'))) {
        event <- paste0("Invalid mode parameter. Valid values include are ",
                        "'text', 'binary', 'object'. See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'write', event = event, level = "Error")
        stop()
      }

      if (mode %in% c('t', 'b')) {
        if (tools::file_ext(path) != "") {
          event <- paste0("Invalid path parameter. Path must be a directory. ",
                          "See ?", class(self)[1], " for further assistance.")
          private$logR$log(method = 'writeText', event = event, level = "Error")
          stop()
        }
        if (mode == 't') {
          io <- IOText$new()
          ext <- ".txt"
        } else {
          io <- IOBin$new()
          ext <- ".bin"
        }
        documents <- corpus$getDocuments()
        lapply(documents, function(d) {
          name <- d$getName()
          p <- file.path(path, paste0(name, ext))
          io$write(path = p, content = d$content)
        })
      } else {
        if (tools::file_ext(path) == "") {
          path <- file.path(path, paste0(corpus$getName(), ".Rds"))
        }
        io <- IOFactory$new()$strategy(path)
        io$write(path = path, content = corpus)
      }

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
