#==============================================================================#
#                               CloneFiles                                     #
#==============================================================================#
#' CloneFiles
#'
#' \code{CloneFiles} Class responsible for cloning Collection and File objects.
#'
#' Class responsible for cloning Collection and File objects.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant object.}
#' }
#'
#' @param x Object to be cloned. Currently supports Collection and File objects.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return A Replicant of the input object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @export
CloneFiles <- R6::R6Class(
  "CloneFiles",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Clone0,

  private = list(

    cloneFile = function(x, path, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }
      path <- file.path(path, x$getFileName())

      out <- File$new(path = path, name = name)
      out <- private$cloneMeta(x, out)
      print("*****************")
      print(path)


      x$copy(path = path)

      event <- paste0("File ", x$getName(), " cloned and stored at ", path, ".")
      x$message(event = event)
      out$message(event = event)
      return(out)
    },

    cloneFiles = function(x, path, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
      }

      out <- FileCollection$new(path = path, name = name)
      out <- private$cloneMeta(x, out)

      # Process File objects
      files <- x$getFiles()
      lapply(files, function(f) {
        file <- private$cloneFile(x = f)
        out <- out$addFile(file)
      })

      event <- paste0("FileCollection ", x$getName()," cloned and stored at ", path, ".")
      x$message(event = event)
      out$message(event = event)
      return(out)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadDependencies()
      invisible(self)
    },
    file = function(x, path, name = NULL) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('File')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'file',
                         event = v$msg, level = "Error")
        stop()
      }

      out <- private$cloneFile(x = x, path = path, name = name)
      return(out)
    },

    collection = function(x, path, name = NULL) {

      # Validate parameters
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('FileCollection')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'collection',
                         event = v$msg, level = "Error")
        stop()
      }

      out <- private$cloneFiles(x = x, path, name = name)
      return(out)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cloneFiles(self)
    }
  )
)
