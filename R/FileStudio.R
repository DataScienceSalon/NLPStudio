#------------------------------------------------------------------------------#
#                                    FileStudio                                #
#------------------------------------------------------------------------------#
#' FileStudio
#'
#' \code{FileStudio}  Class responsible for performing operations on files.
#'
#' Class registers files and filesets, and manipulates them, usually at raw binary files.
#'
#' @param x File or FileSet object
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
  inherit = Super,

  private = list(

    ..fileSet = character(),

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
      quant$size <- file.info(path)[['size']]
      quant$lines <- length(text)
      quant <- c(quant, encodings)
      return(quant)

    },

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

    getFilePaths = function(path) {
      if (isDirectory(path)) {
        files <- list.files(path, full.names = TRUE)
      } else {
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }
      return(files)
    },

    validate = function(x, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('File', 'FileSet'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
    }
  ),

  public = list(

    initialize = function() {


      private$loadServices()
      invisible(self)
    },

    build = function(path, name = NULL) {

      fileSet <- FileSet$new(name = name)

      filePaths <- private$getFilePaths(path)
      for (i in 1:length(filePaths)) {
        file <- File$new(path = filePaths[i])
        fileSet$addFile(file)
      }
      return(fileSet)
    },

    copy = function(x, to) {

      private$validate(x, methodName = 'copy')

      if (class(x)[1] == 'File') {
        path <- x$getFilePath()
        file.copy(from = path, to = to, recursive = TRUE, overwrite = TRUE)

      } else {
        files <- x$getFiles()
        for (i in 1:length(files)) {
          path <- files[[i]]$getFilePath()
          file.copy(from = path, to = to, recursive = TRUE, overwrite = TRUE)
        }
      }
    },

    move = function(x, to) {

      private$validate(x, methodName = 'move')

      if (class(x)[1] == 'File') {
        path <- x$getFilePath()
        file.rename(from = path, to = to)

      } else {
        files <- x$getFiles()
        for (i in 1:length(files)) {
          path <- files[[i]]$getFilePath()
          file.rename(from = path, to = to)
        }
      }
    },

    inspect = function(fileSet) {
      files <- fileSet$getFiles()
      quant <- rbindlist(lapply(files, function(f) {
        quant <- private$inspectFile(f)
        f$setQuant(quant)
        fileSet$addFile(f)
        quant
      }))
      fileSet$setQuant(quant)
      return(fileSet)
    },

    repair = function(fileSet, newPath, codes = NLPStudio:::nonPrintables) {

      newFileSet <- Clone$new()$this(x = fileSet, reference = FALSE, path = newPath)

      files <- newFileSet$getFiles()
      for (i in 1:length(files)) {
        private$repairFile(files[[i]],codes)
      }

      name <- fileSet$getName()
      name <- paste0(name, " (repaired)")
      newFileSet$setName(name)
      # Log it
      event <- paste0("Executed ", class(self)[1], " on FileSet ", name, ". ")
      private$logR$log(method = 'execute', event = event)

      return(newFileSet)
    }
  )
)
