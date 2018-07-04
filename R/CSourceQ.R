#' CSourceQ
#'
#' \code{CSourceQ} Sources a Corpus object from a Quanteda object.
#'
#' @section Methods:
#'  \itemize{
#'   \item{\code{new(x, name = NULL)}}{Initializes an object of the CSourceQ class.}
#'   \item{\code{source()}}{Executes the process of sourcing the Corpus object.}
#'  }
#'
#' @param name Optional character vector indicating name for Corpus object.
#' @param x Character vector or a list of character vectors containing text.
#'
#' @examples
#' text <- c("The firm possesses unparalleled leverage in Washington,
#' thanks in part to its track record of funneling executives into
#' senior government posts. Even the Trump administration, which
#' rode a populist wave to electoral victory, is stocked with
#' Goldman alumni, including Treasury Secretary Steven Mnuchin and
#' the departing White House economic adviser Gary D. Cohn.",
#' "Goldman is also an adviser to many of America’s — and the
#' world’s — largest companies, ranging from stalwarts like Walt
#' Disney to upstarts like Uber.")
#'
#' corpus <- CSource$new(x = txt, name = "Goldman")$vector()
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Source Classes
#' @export
CSourceQ <- R6::R6Class(
  classname = "CSourceQ",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CSource0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Instantiation Method                              #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$loadServices()
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Execute Method                                 #
    #-------------------------------------------------------------------------#
    source = function(x, name = NULL) {

      # Validate text
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('corpus'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'source',
                         event = v$msg, level = "Error")
        stop()
      }

      corpus <- ConverterQ$new()$convert(x)
      if (!is.null(name))  corpus$setName(name = name)
      corpus$setMeta(key = 'source', value = 'Quanteda Corpus',
                     type = 'f')

      event <- paste0("Corpus ", corpus$getName(), " sourced from ",
                      "a quanteda corpus object.")
      corpus$message(event = event)

      return(corpus)
    },
    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$csourceQuanteda(self)
    }
  )
)
