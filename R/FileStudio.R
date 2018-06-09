#------------------------------------------------------------------------------#
#                                    FileStudio                                #
#------------------------------------------------------------------------------#
#' FileStudio
#'
#' \code{FileStudio}  Class responsible for performing operations on files.
#'
#' Class registers files and filesets, and manipulates them, usually at raw binary files.
#'
#' @param x FileSet object
#' @param path Character string containing a relative path to a file or
#' directory.  It may also contain a glob expression that resolves to one
#' or several files.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @export
FileStudio <- R6::R6Class(
  classname = "FileStudio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileStudio0,

  private = list(

    ..fileSource = character(),
    #-------------------------------------------------------------------------#
    #                          Inspect File Method                            #
    #-------------------------------------------------------------------------#
    inspectFile = function(f) {

      # Read Data
      path <- f$getMeta(key = 'path')
      text <- readLines(path)

      # Inspect encodings
      e <- as.list(as.data.frame(table(stringi::stri_enc_mark(text))))
      encodings <- as.list(e$Freq)
      names(encodings) <- e$Var1

      # Capture quantitative rsults
      quant <- list()
      quant$bytes <- file.info(path)[['size']]
      quant$lines <- length(text)
      quant <- c(quant, encodings)
      return(quant)

    },
    #-------------------------------------------------------------------------#
    #                          Repair File Method                             #
    #-------------------------------------------------------------------------#
    repairFile = function(file, codes = NLPStudio:::nonPrintables) {

      path <- file$getFilePath()

      ioBin <- IOBin$new()
      ioTxt <- IOText$new()

      # Read and FileStudio content
      content <- ioBin$read(path = path)
      for (i in 1:length(codes)) {
        content[content == as.raw(codes[i])] = as.raw(0x20)
      }

      # Save to temp file, then re-read
      d <- tempfile(fileext = '.txt')
      ioBin$write(path = d, content = content)
      content <- ioTxt$read(path = d)
      unlink(d)

      # Save as text file
      ioTxt$write(path = path, content = content)

      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                         Validate FileSet                                #
    #-------------------------------------------------------------------------#
    validateFileSet = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('FileSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                         Validate FileSource                             #
    #-------------------------------------------------------------------------#

    validateFileSource = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('FileSourceTxt', 'FileSourceCsv',
                                               'FileSourceXml', 'FileSourceHTML',
                                               'FileSourceJSON'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    },

    createFileSet = function(path, name = NULL) {

      private$validatePath(param = path, paramName = 'path',
                           methodName = 'createFileSet')

      fileSet <- FileSet$new(name)
      fileSet$setMeta(key = 'path', value = path, type = 'f')
      files <- private$getFilePaths(path)
      for (i in 1:length(files)) {
        file <- File$new(files[[i]])
        fileSet$addFile(file)
      }
      return(fileSet)
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
    #                             Source Method                               #
    #-------------------------------------------------------------------------#
    source = function(fileSource, name = NULL) {

      private$validateFileSource(param = fileSource, paramName = 'fileSource',
                                 methodName = 'source')
      path <- fileSource$source()
      fileSet <- private$createFileSet(path, name)

      return(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                              Read Method                                #
    #-------------------------------------------------------------------------#
    read = function(x) {
      private$validateFileSet(param = x, paramName = 'x', methodName = 'read')
      files <- x$getFiles()
      content <- lapply(files, function(f) {
        file <- f$getFilePath()
        io <- IOFactory$new()$strategy(file)
        io$read(file)
      })
      return(content)
    },

    #-------------------------------------------------------------------------#
    #                             Write Method                                #
    #-------------------------------------------------------------------------#
    write = function(x, content, fileName) {
      private$validateFileSet(param = x, paramName = 'x', methodName = 'read')
      io <- IOFactory$new()$strategy(fileName)
      filePath <- x$getFilePath()
      filePath <- file.path(filePath, fileName)
      io@write(path = filePath, content = content)
      file <- File$new(filePath)
      x$addFile(file)
      return(x)
    },


    #-------------------------------------------------------------------------#
    #                              Copy Method                                #
    #-------------------------------------------------------------------------#
    copy = function(x, to) {

      private$validateFileSet(param = x, paramName = 'x', methodName = 'copy')
      private$validatePath(param = to, paramName = 'to', methodName = 'copy')

      if (!file.exists(to)) dir.create(to, recursive = TRUE)

      files <- x$getFiles()
      for (i in 1:length(files)) {
        path <- files[[i]]$getFilePath()
        file.copy(from = path, to = to, recursive = FALSE, overwrite = TRUE)
      }
      name <- paste0("Copy of ", x$getName())
      fileSet <- private$createFileSet(to, name)

      return(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                             Move Method                                 #
    #-------------------------------------------------------------------------#
    move = function(x, to) {

      private$validateFileSet(param = x, paramName = 'x', methodName = 'move')
      private$validateChar(param = to, paramName = 'to', methodName = 'move')

      if (!file.exists(to)) dir.create(to, recursive = TRUE)

      files <- x$getFiles()
      for (i in 1:length(files)) {
        filePath <- files[[i]]$getFilePath()
        fileName <- files[[i]]$getFileName()
        newPath <- file.path(to, fileName)
        file.rename(from = filePath, to = newPath)
        file <- files[[i]]$setFilePath(newPath)
        x$addFile(file)
        if (length(list.files(path = dirname(filePath))) == 0) unlink(dirname(filePath), recursive = TRUE)
      }
      return(x)
    },
    #-------------------------------------------------------------------------#
    #                           Inspect Method                                #
    #-------------------------------------------------------------------------#
    inspect = function(x) {
      files <- x$getFiles()
      lapply(files, function(f) {
        quant <- private$inspectFile(f)
        f$setQuant(quant)
        fileSet$addFile(f)
        f$summary(section = c('i', 'q'))
      })
      invisible(x)
    },
    #-------------------------------------------------------------------------#
    #                           Repair Method                                 #
    #-------------------------------------------------------------------------#
    repair = function(x, codes = NLPStudio:::nonPrintables) {

      name <- x$getName()

      files <- x$getFiles()
      for (i in 1:length(files)) {
        private$repairFile(files[[i]],codes)
      }

      # Log it
      event <- paste0("Repaired FileSet ", name, ". ")
      private$logR$log(method = 'execute', event = event)

      return(x)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
