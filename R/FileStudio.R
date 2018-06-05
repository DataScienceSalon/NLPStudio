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
  inherit = Super,

  private = list(

    ..fileSet = character(),
    #-------------------------------------------------------------------------#
    #                           Source File Method                            #
    #-------------------------------------------------------------------------#
    sourceFile = function(path) {
      ext <- tolower(tools::file_ext(path))
      file <- switch(ext,
             csv = FileSourceCsv$new()$source(path),
             docx = FileSourceDocx$new()$source(path),
             html = FileSourceHtml$new()$source(path),
             json = FileSourceJSON$new()$source(path),
             txt  = FileSourceTxt$new()$source(path),
             xml = FileSourceXml$new()$source(path)
             )
      return(file)
    },
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
    #                          Get File Paths Method                          #
    #-------------------------------------------------------------------------#
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
    #-------------------------------------------------------------------------#
    #                        Validate Object Method                           #
    #-------------------------------------------------------------------------#
    validateObject = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('FileSet', 'File'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      }
      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                       Validate Character Method                         #
    #-------------------------------------------------------------------------#
    validateChar = function(param, paramName, methodName) {
      private$..params <- list()
      private$..params$classes$name <- list(paramName)
      private$..params$classes$objects <- list(param)
      private$..params$classes$valid <- list(c('character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        stop()
      } else if (length(param) > 1) {
        event <- paste0("Invalid ", paramName, " parameter.  Must be a single character string.")
        private$logR$log(method = methodName, event = event, level = "Error")
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
      private$loadServices()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Build Method                                #
    #-------------------------------------------------------------------------#
    build = function(path, name = NULL) {

      # Validation
      if (!file.exists(path)) {
        event = paste0("Invalid path, ", path, " does not exist.")
        private$logR$log(method = 'build', event = event, level = "Error")
        stop()
      }
      if (!is.null(name)) private$validateChar(param = name, paramName = 'name',
                                               methodName = 'build')

      fileSet <- FileSet$new(name = name)
      filePaths <- private$getFilePaths(path)

      for (i in 1:length(filePaths)) {
        file <- private$sourceFile(filePaths[i])
        fileSet$addFile(file)
      }
      return(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                              Copy Method                                #
    #-------------------------------------------------------------------------#
    copy = function(x, to) {

      private$validateObject(param = x, paramName = 'x', methodName = 'copy')
      private$validateChar(param = to, paramName = 'to', methodName = 'copy')

      if (!file.exists(to)) dir.create(to, recursive = TRUE)

      files <- x$getFiles()
      for (i in 1:length(files)) {
        path <- files[[i]]$getFilePath()
        file.copy(from = path, to = to, recursive = FALSE, overwrite = TRUE)
      }

      # Create new FileSet object
      fileSet <- self$build(path = to, name = paste0(x$getName(), " (Copy)"))
      return(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                             Move Method                                 #
    #-------------------------------------------------------------------------#
    move = function(x, to) {

      private$validateObject(param = x, paramName = 'x', methodName = 'move')
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
    inspect = function(fileSet) {
      files <- fileSet$getFiles()
      lapply(files, function(f) {
        quant <- private$inspectFile(f)
        f$setQuant(quant)
        fileSet$addFile(f)
        f$summary(section = c('i', 'q'))
      })
      invisible(fileSet)
    },
    #-------------------------------------------------------------------------#
    #                           Repair Method                                 #
    #-------------------------------------------------------------------------#
    repair = function(fileSet, newPath, codes = NLPStudio:::nonPrintables) {

      name = paste0(fileSet$getName(), " (Repaired)")
      newFileSet <- Clone$new()$this(x = fileSet, reference = TRUE,
                                     path = newPath)
      newFileSet$setName(name = name)

      files <- newFileSet$getFiles()
      for (i in 1:length(files)) {
        private$repairFile(files[[i]],codes)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on FileSet ", name, ". ")
      private$logR$log(method = 'execute', event = event)

      return(newFileSet)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
