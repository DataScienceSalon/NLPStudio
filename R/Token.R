#' Token
#'
#' \code{Token} Token class. Contains character, word, sentence, or paragraph tokens.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Token Family of Classes
#' @export
Token <- R6::R6Class(
  classname = "Token",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Corpus,

  public = list(

    initialize = function(name = NULL) {

      private$loadServices()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$token(self)
    }
  )
)
