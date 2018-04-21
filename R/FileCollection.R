#' FileCollection
#'
#' \code{FileCollection} Class representation of a collection of files.
#'
#' Class representing a collection of files. FileCollections are created from
#' raw text and undergo treatments to address anamolies at the file level
#' before Corpus creation and preprocessing.
#'
#' @usage rawFiles <- FileCollection$new(name = 'rawCollection', path = './raw')
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the FileCollection class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the FileCollection object.}
#'  }
#'
#' @param name Character string containing the name for the FileCollection object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the FileCollections
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return FileCollection object, containing the FileCollection text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File Classes
#' @export
FileCollection <- R6::R6Class(
  classname = "FileCollection",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Collection0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(path, name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$meta$set(key = 'name', value = ifelse(is.null(name),
                                                    basename(path),
                                                    name),
                       type = 'descriptive')

      private$meta$set(key = 'path', value = path, type = 'functional')
      private$meta$set(key = 'directory', value = path, type = 'functional')

      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Document Management                           #
    #-------------------------------------------------------------------------#
    getFiles = function() { private$..documents },
    getPath = function() {private$meta$get(key = 'path')},
    getDirectory = function() {private$meta$get(key = 'directory')},
    addFile = function(x) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('File'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'addFile',
                         event = v$msg, level = "Error")
        stop()
      }
      private$attach(x)

      invisible(self)

    },
    removeFile = function(x) {
      private$detach(x)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Move/Copy Methods                             #
    #-------------------------------------------------------------------------#
    move = function(path, overwrite = FALSE) {
      for (i in 1:length(private$..documents)) {
        filePath <- file.path(path, private$..documents[[i]]$getFileName())
        private$..documents[[i]]$move(path = filePath, overwrite = overwrite)
      }
      event <- paste0("Moved FileCollection, ", self$getName(), ", from ",
                      self$getPath(), " to ", path, ".")
      private$meta$modified(event = event)
      private$logR$log(method = 'move', event = event)
      private$meta$set(key = 'path', value = path, type = 'f')
      private$meta$set(key = 'directory', value = path, type = 'f')
      invisible(self)
    },

    copy = function(path, overwrite = FALSE) {
      for (i in 1:length(private$..documents)) {
        filePath <- file.path(path, private$..documents[[i]]$getFileName())
        private$..documents[[i]]$copy(path = filePath, overwrite = overwrite)
      }
      event <- paste0("Copied FileCollection, ", self$getName(), ", from ",
                      self$getPath(), " to ", path, ".")
      private$meta$accessed(event = event)
      private$logR$log(method = 'copy', event = event)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {
      content <- lapply(private$..documents, function(f) {
        f$read()
      })
      return(content)
    },

    write = function(x) {
      if (length(x) != length(private$..documents)) {
        event <- paste0("Unable to write 'x' to files. The variable length ",
                        length(x), ", must match the number ",
                        "of files in the FileCollection, which is ",
                        length(private$..documents), ". See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(method = 'write', event = event, level = 'Error')
        stop()
      } else {
        for (i in 1:length(private$..documents)) {
          private$..documents[[i]]$write(x[[1]])
        }
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileCollection(self)
    }
  )
)
