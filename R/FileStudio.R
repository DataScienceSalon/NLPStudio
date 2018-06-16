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
    ..fileSet = character(),
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
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x) {
      private$loadServices(name = 'FileStudio')

      if (!private$validateFileSource(param = x, paramName = 'fileSource',
                                 methodName = 'initialize')) stop()

      private$..fileSource <- x

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                             Source Method                               #
    #-------------------------------------------------------------------------#
    buildFileSet = function(path, name = NULL, overwrite = FALSE) {

      private$..fileSet <- private$..fileSource$buildFileSet(destination = path,
                                                             name = name,
                                                             overwrite = overwrite)

      event <- paste0("Sourced FileSet from ", private$..fileSource$getOrigin(), ".")
      private$..fileSet$message(event)
      private$logR$log(method = 'source', event = event, level = "Info")
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Inspect Method                                #
    #-------------------------------------------------------------------------#
    inspect = function() {
      files <- private$..fileSet$getFiles()

      lapply(files, function(f) {
        name <- f$getName()
        quant <- private$inspectFile(f)
        f$setQuant(quant)
        private$..fileSet$addFile(f)
      })

      event <- paste0("Inspected FileSet ", private$..fileSet$getName(), ".")
      private$..fileSet$message(event)
      private$logR$log(method = 'inspect', event = event, level = "Info")
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                           Repair Method                                 #
    #-------------------------------------------------------------------------#
    repair = function(codes = NLPStudio:::nonPrintables) {

      name <- private$..fileSet$getName()

      files <- private$..fileSet$getFiles()
      for (i in 1:length(files)) {
        private$repairFile(files[[i]],codes)
      }

      # Log it
      event <- paste0("Repaired FileSet ", name, ". ")
      private$..fileSet$message(event)
      private$logR$log(method = 'repair', event = event)

      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                            Query Methods                                #
    #-------------------------------------------------------------------------#
    getFileSet = function() private$..fileSet,
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio(self)
    }
  )
)
