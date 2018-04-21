#==============================================================================#
#                               FileStudio0                                    #
#==============================================================================#
#' FileStudio0
#'
#' \code{FileStudio0} Abstract class  for the FileStudio family of classes.
#'
#' This abstract class defines a common interface and methods of the FileStudio
#' family of classes.
#'
#' @template fileStudioParams
#' @template fileStudioMethods
#' @template fileStudioClasses
#' @template fileStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Classes
#' @export
FileStudio0 <- R6::R6Class(
  classname = "FileStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,


  private = list(
    ..in = character(),
    ..out = character()
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      if ("FileCollection" %in% class(private$..out)) {
        private$..out <- private$processCollection()

      } else {
        private$..out <- private$processFile(private$..out)
      }

      # Log it
      event <- paste0("Executed ", class(self)[1], " on ",
                                private$..out$getName(), ". ")
      private$..out$message(event)
      private$logR$log(method = 'execute', event = event)

      return(private$..out)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$fileStudio0(self)
    }
  )
)
