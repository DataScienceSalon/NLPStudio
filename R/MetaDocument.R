#' MetaDocument
#'
#' \code{MetaDocument} Metadata class for the Document family of classes.
#'
#' Provides the methods for creating, managing, and reporting metadata for the
#' Document family of classes.  This class includes the base functionality
#' from the Meta0 class, as well as functionality to create, manage
#' and report custom document metadata.
#'
#' @section MetaDocument Methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented for this base class.}
#'   \item{\code{getIdentity(key = NULL)}}{Returns identity metadata such as
#'   family, class, identifier, and name of an object. If no key is
#'   designated, all identity variables are returned.}
#'   \item{\code{setIdentity(x, name = NULL, purpose = NULL)}}{Sets the identify
#'   of an object. The identity variables are family, class, name, and an
#'   identifier. The family is the object's parent class and the identifier
#'    is randomly generated and assigned to each object.}
#'   \item{\code{checkIdentity(id = NULL, name = NULL, class = NULL,
#'   family = NULL, purpose = NULL)}}{A logical check to determine if an
#'   object matches the query parameters.}
#'   \item{\code{getStats(key = NULL)}}{Obtains descriptive statistics data
#'   assigned to the object.}
#'   \item{\code{setStats(key, value)}}{Sets descriptive statistics data
#'   for an object.}
#'   \item{\code{getState()}}{Obtains an objects current state information.}
#'   \item{\code{modified(event = NULL)}}{Sets state datetime parameters
#'   when an object is modified.}
#'   \item{\code{summary()}}{Method for summarizing an objects metadata.}
#'   \item{\code{getCustom(key = NULL)}}{Obtains custom metadata which has been
#'   assigned to the object.}
#'   \item{\code{setCustom(key, value)}}{Sets custom metadata using key value
#'   pairs.}
#' }
#'
#' @template metadataParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
MetaDocument <- R6::R6Class(
  classname = "MetaDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta0,

  private = list(
    ..custom = NULL
  ),

  public = list(

    initialize = function() {

      private$loadDependencies()
      private$..state$current <- paste0("Instantiated.")
      private$..state$creator <- Sys.info()[['user']]
      private$..state$created <- Sys.time()
      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                         Custom Metadata Methods                         #
    #-------------------------------------------------------------------------#
    getCustom = function(key = NULL) {

      if (is.null(private$..custom)) {
        event <- paste0("No custom metadata exists for this object.")
        private$logR$log(cls = class(self)[1], method = 'getCustom',
                         event = event, level = "Warn")
      } else if (is.null(key)) {
        return(private$..custom)
      } else if (!is.null(private$..custom[[key]])) {
        return(private$..custom[[key]])
      } else {
        event <- paste0("Key, '", key, "', is not a valid custom metadata ",
                        "variable. See ?", class(self)[1],
                        " for further assistance.")
        private$logR$log(cls = class(self)[1], method = 'getCustom',
                         event = event, level = "Warn")
      }
      invisible(self)
    },

    setCustom = function(key, value) {

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- TRUE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(cls = class(self)[1], method = 'setCustom',
                         event = v$msg, level = 'Error')
        stop()
      }

      if (is.null(private$..custom)) private$..custom <- list()
      for (i in 1:length(key)) {
        private$..custom[[key[i]]] <- value[i]
      }
      invisible(self)
    },

    checkCustom = function(key, value) {

      if (is.null(private$..custom)) return(FALSE)

      private$..params$kv$key <- key
      private$..params$kv$value <- value
      private$..params$kv$equalLen <- FALSE
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(cls = class(self)[1], method = 'setCustom',
                         event = v$msg, level = 'Error')
        stop()
      }

      if (length(value) > length(key)) key <- rep(key, length(value))
      for (i in 1:length(key)) {
        if (!is.null(private$..custom[[key[i]]])) {
          if (private$..custom[[key[i]]] %in% value[i]) {
            return(TRUE)
          }
        }
      }
      return(FALSE)
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function() {

      created <- format(private$..state$created, format="%Y-%m-%d at %H:%M:%S")
      s <- c(private$..identity, private$..custom, private$..stats,
             creator = private$..state$creator,
             created = created)
      s <- Filter(Negate(is.null), s)

      return(s)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$metaDocument(self)
    }
  )
)
