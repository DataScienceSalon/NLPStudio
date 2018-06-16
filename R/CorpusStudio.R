#------------------------------------------------------------------------------#
#                               CorpusStudio                                   #
#------------------------------------------------------------------------------#
#' CorpusStudio
#'
#' \code{CorpusStudio}  Class responsible for building and manipulating Corpus objects.
#'
#' @param x a series of character vectors, each containing the text for a single
#' document, a FileSet object containing .txt files, a character string
#' containing the directory containing .txt files, a quanteda corpus object, or
#' a tm VCorpus, or tm SimpleCorpus object.
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
#' @param overWrite Logical, Used in the read method. If TRUE, any text or binary
#' files read will overwrite existing versions of the same document source.
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
    ..corpus = character(),

    validate = function(corpus, methodName) {

      private$..params <- list()
      private$..params$classes$name <- list('corpus')
      private$..params$classes$objects <- list(corpus)
      private$..params$classes$valid <- list('Corpus')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices(name = 'CorpusStudio')
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(x, name = NULL) {

      sourceType <- class(x)[1]
      private$..params <- list()
      private$..params$discrete$variables = list('sourceType')
      private$..params$discrete$values = list(sourceType)
      private$..params$discrete$valid = list(c('character', 'FileSet', 'corpus',
                                               'VCorpus', 'SimpleCorpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'build', event = v$msg, level = "Error")
        stop()
      }

      if (sourceType == 'character') {
        if (isDirectory(x)) {
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

      event <- paste0("Constructed Corpus from ", sourceType, ".")
      corpus$message(event)
      private$logR$log(method = 'build', event = event, level = "Info")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Tokenize Method                             #
    #-------------------------------------------------------------------------#
    addDocuments = function()

    #-------------------------------------------------------------------------#
    #                      Corpus Tokenize Method                             #
    #-------------------------------------------------------------------------#
    tokenize = function(corpus, name = NULL, tokenUnit = 'sentence') {

      # Validation
      private$validate(corpus, methodName = 'tokenize')

      private$..params <- list()
      private$..params$discrete$variables = list('tokenUnit')
      private$..params$discrete$values = list(tokenUnit)
      private$..params$discrete$valid = list(c('word', 'sentence', 'paragraph',
                                               'w', 's', 'p'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'tokenize', event = v$msg, level = "Error")
        stop()
      }

      # Obtain documents and tokenize
      token <- Tokenizer$new()$execute(corpus, name, tokenUnit)

      event <- paste0("Created ", tokenUnit, " object from Corpus.")
      corpus$message(event)
      private$logR$log(method = 'tokenize', event = event, level = "Info")
      return(token)
    },

    #-------------------------------------------------------------------------#
    #                        Corpus Sample Method                             #
    #-------------------------------------------------------------------------#
    sample = function(corpus, n, name = NULL, stratify = TRUE, replace = FALSE,
                      seed = NULL) {
      # Validation
      private$validate(corpus, methodName = 'sample')

      private$..params <- list()
      private$..params$logicals$variables <- c('stratify', 'replace')
      private$..params$logicals$values <- c(stratify, replace)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'sample', event = v$msg, level = "Error")
        stop()
      }

      # Obtain samples
      sampler <- Sample$new()
      corpus <- sampler$execute(x = corpus, n, name, stratify,
                             replace, seed)

      type <- ifelse(stratify == TRUE, 'stratified', 'non-stratified')
      event <- paste0("Created ", type, " sample from Corpus.")
      corpus$message(event)
      private$logR$log(method = 'sample', event = event, level = "Info")

      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                         Corpus Split Method                             #
    #-------------------------------------------------------------------------#
    split = function(corpus, name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL) {

      # Validate
      private$validate(corpus, methodName = 'split')

      private$..params <- list()
      private$..params$classes$name <- list('train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c("logical"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'split', event = v$msg, level = "Error")
        stop()
      }

      # Render splits
      splitter <- Split$new()
      cvSet <- splitter$execute(corpus, name, train, validation, test,
                                stratify, seed)

      event <- paste0("Created cross-validation set from Corpus.")
      cvSet$message(event)
      private$logR$log(method = 'split', event = event, level = "Info")

      return(cvSet)
    },

    #-------------------------------------------------------------------------#
    #                   Corpus kFold Split Method                             #
    #-------------------------------------------------------------------------#
    kFold = function(corpus, k = 10, name = NULL, train = 0.75, validation = 0,
                     test = 0.25,  stratify = TRUE, seed = NULL)  {

      # Validate
      private$validate(corpus, methodName = 'kFold')

      private$..params <- list()
      private$..params$classes$name <- list('k', 'train', 'validation',
                                            'test', 'stratify')
      private$..params$classes$objects <- list(k, train, validation,
                                               test, stratify)
      private$..params$classes$valid <- list(c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c('integer', 'numeric'),
                                             c("logical"))
      private$..params$logicals$variables <- c('stratify')
      private$..params$logicals$values <- c(stratify)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'kFold', event = v$msg, level = "Error")
        stop()
      }
      splitter <- SplitKFold$new()
      cvKFold <- splitter$execute(corpus, k, name, train, validation,
                                  test,  stratify, seed)

      event <- paste0("Created k-fold cross-validation set from Corpus.")
      cvKFold$message(event)
      private$logR$log(method = 'kFold', event = event, level = "Info")

      return(cvKFold)
    },
    #-------------------------------------------------------------------------#
    #                        Corpus Clean Method                              #
    #-------------------------------------------------------------------------#
    clean = function(corpus, config, name = NULL) {

      # Validation
      private$validate(corpus, methodName = 'clean')
      private$..params <- list()
      private$..params$classes$name <- list('config')
      private$..params$classes$objects <- list(config)
      private$..params$classes$valid <- list(c('TextConfig'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'clean', event = v$msg, level = "Error")
        stop()
      }

      ts <- TextStudio$new()
      corpus <- ts$loadConfig(config)$execute(corpus, name)

      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                        Read Documents Method                            #
    #-------------------------------------------------------------------------#
    read = function(corpus, path, overwrite = FALSE) {

      private$validate(corpus, methodName = 'read')

      filePaths <- NLPStudio:::listFiles(path)
      if (length(filePaths > 0)) {
        for (i in 1:length(filePaths)) {
          if (overwrite) {
            idx <- private$search(key = 'source', value = filePaths[[i]])
            doc <- private$..children[[idx]]
            corpus$removeDocument(doc)
          }
          name <- tools::file_path_sans_ext(basename(filePaths[i]))
          io <- IOFactory$new()$strategy(filePaths[i])
          content <- io$read(filePaths[i])
          document <- Document$new(x = content, name = name)
          document$setMeta(key = 'source', value = filePaths[i], type = 'f')
          corpus$addDocument(document)
        }
        quant <- corpus$getDocMeta(type = 'q')[[1]]
        if (nrow(quant) > 0) {
          keys <- c(names(quant), 'documents')
          values <- c(colSums(quant), nrow(quant))
          corpus$setMeta(key = keys, value = values, type = 'q')
        }
      } else {
        event <- paste0("No files found at path or glob")
        private$logR$log(method = 'read', event = event, level = "Warn")
      }

      event <- paste0("Read and added documents to Corpus.")
      corpus$message(event)
      private$logR$log(method = 'read', event = event, level = "Info")

      return(corpus)
    },


    #-------------------------------------------------------------------------#
    #                        Corpus Write Method                              #
    #-------------------------------------------------------------------------#
    write = function(corpus, path, format = "txt", overwrite = FALSE) {

      # Validate
      private$validate(corpus, methodName = 'write')

      private$..params <- list()
      private$..params$discrete$variables = list('format')
      private$..params$discrete$values = list(tolower(format))
      private$..params$discrete$valid = list(c('txt', 'csv', 'rdata',
                                               'rds', 'bin'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'write', event = v$msg, level = "Error")
        stop()
      }

      # Validate that path is a directory
      if (tools::file_ext(path) != "") {
        event <- paste0("Invalid path parameter. Path must be a directory. ",
                        "See ?", class(self)[1], " for further assistance.")
        private$logR$log(method = 'write', event = event, level = "Error")
        stop()
      }

      io <- switch(format,
                   txt = IOText$new(),
                   csv = IOCSV$new(),
                   rdata = IORdata$new(),
                   rds = IORds$new(),
                   bin = IOBin$new())

      ext <- switch(format,
                    txt = '.txt',
                    csv = '.csv',
                    rdata = '.RData',
                    rds = '.Rds',
                    bin = '.bin')

      docs <- corpus$getDocuments()
      lapply(docs, function(d) {
        filePath <- d$getMeta(key = 'path')
        if (length(filePath) == 0) {
          filePath <- file.path(path, paste0(d$getName(), ext))
        } else {
          filePath <- file.path(path, paste0(basename(filePath), ext))
        }
        if (overwrite == FALSE) {
          if (file.exists(filePath)) {
            event <- paste0("File ", filePath, " already exists. Change the ",
                            "path or set the 'overwrite' variable to TRUE ",
                            "to overwrite the file.")
            private$logR$log(method = 'write', event = event, level = "Error")
            stop()
          } else {
            io$write(content = d$content, path = filePath)
          }
        }
      })

      event <- paste0("Wrote Corpus documents to ", path, ".")
      corpus$message(event)
      private$logR$log(method = 'write', event, level = "Info")

      return(corpus)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusStudio(self)
    }
  )
)
