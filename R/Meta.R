#' Meta
#'
#' \code{Meta} Base class for metadata objects.
#'
#' Contains object metadata and the methods for adding, removing, and reporting
#' metadata.  Also includes convenience methods for updating state metadata
#' relating to datetimes of creating, update, and access.
#'
#' @section Meta methods:
#' @template metadataMethods.R
#'
#' @template metadataParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Metadata Classes
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..identity = list(),
    ..state = list()
  ),

  public = list(

    initialize = function() { stop("This method is not implemented for this base class.") },

    #-------------------------------------------------------------------------#
    #                            Identity Methods                             #
    #-------------------------------------------------------------------------#
    getIdentity = function(key = NULL) {
      if (is.null(key)) {
        return(private$..identity)
      } else if (!is.null(private$..identity[[key]])) {
        return(private$..identity[[key]])
      } else {
        event <- paste0("Variable, ", key, " is not a valid identity metadata ",
                        "variable.")
        private$logR$log(cls = private$..identity$class, method = "getIdentity",
                         level = "Warn")
      }
    },

    setIdentity = function(x, name = NULL, purpose = NULL) {

      # Designate family, class and purpose
      private$..identity$family <- class(x)[2]
      private$..identity$class <- class(x)[1]
      private$..identity$purpose <- purpose

      # Creates unique identifier
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      private$..identity$id <- hashids::encode(as.integer(Sys.time()) * 1000000 +
                                  sample(1:1000, 1, replace = TRUE), settings)

      # Designate/create object name
      private$..identity$name <- ifelse(is.null(name),
                                        paste0(private$..identity$class,
                                               " (", toupper(private$..identity$id),
                                               ")"), name)
      invisible(private$..identity)
    },

    #-------------------------------------------------------------------------#
    #                             State Methods                               #
    #-------------------------------------------------------------------------#
    getState = function() { return(private$..state) },

    modified = function(event = NULL) {

      private$..state$modifiedBy <- Sys.info()[["user"]]
      private$..state$current <- ifelse(is.null(event), "Modified.", event)
      private$..state$modified <- Sys.time()

      invisible(self)
    }
  )
)
