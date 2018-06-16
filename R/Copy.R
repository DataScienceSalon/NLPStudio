#==============================================================================#
#                                   Copy                                       #
#==============================================================================#
#' Copy
#'
#' \code{Copy0} Abstract class for the Copy family of classes.
#'
#' Abstract class that defines the interface and common methods for the
#' Copy family of classes.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x, name = NULL)}}{Instantiates the factory.}
#'  \item{\code{execute()}}{Returns the replicant object.}
#' }
#'
#' @param x Object to be Copyd.
#' @param name Character string containing the name to be assigned to the
#' replicant.
#'
#' @return A Replicant of the input object
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Copy Classes
#' @export
Copy <- R6::R6Class(
  classname = "Copy",
  lock_class = FALSE,
  lock_objects = FALSE,
  inherit = Super,

  private = list(

    copyMeta = function(x, to) {

      to$setName(name = x$getName())

      # Copy descriptive
      descriptive <- x$getMeta(type = 'd')
      if (length(descriptive) > 0){
        keys <- names(descriptive)
        for (i in 1:length(descriptive)) {
          to$setMeta(key = keys[i], value = descriptive[[i]], type = 'd')
        }
      }

      # Copy functional
      functional <- x$getMeta(type = 'f')
      if (length(functional) > 0) {
        keys <- names(functional)
        for (i in 1:length(functional)) {
          to$setMeta(key = keys[i], value = functional[[i]], type = 'f')
        }
      }
      return(to)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$loadServices()
      invisible(self)
    },
    #-------------------------------------------------------------------------#
    #                            Factory Method                               #
    #-------------------------------------------------------------------------#
    this = function(x, to) {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('x', 'to')
      private$..params$classes$objects <- list(x, to)
      private$..params$classes$valid <- list(c('Corpus','Document'),
                                             c('Corpus', 'Document',
                                               'Token', 'Fold'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'this',
                         event = v$msg, level = "Error")
        stop()
      }

      to <- private$copyMeta(x, to)
      return(to)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$copy(self)
    }
  )
)
