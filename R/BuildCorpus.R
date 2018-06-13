#------------------------------------------------------------------------------#
#                               BuildCorpus                                    #
#------------------------------------------------------------------------------#
#' BuildCorpus
#'
#' \code{BuildCorpus} Encapsulates the command to source a FileSet object.
#'
#' @param x FileBuildCorpus Object
#' @param fileStudio FileStudio object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family FileStudio Command Classes
#' @export
BuildCorpus <- R6::R6Class(
  classname = "BuildCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Cmd0,

  private = list(
    ..corpusStudio = character(),
    ..name = character()
  ),

  public = list(
    initialize = function(corpusStudio, name = NULL) {

      private$loadServices(name = 'BuildCorpus')

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('corpusStudio')
      private$..params$classes$objects <- list(corpusStudio)
      private$..params$classes$valid <- list(c('CorpusStudio'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'initialize', event = v$msg, level = 'Error')
        stop()
      }

      private$..corpusStudio <- corpusStudio
      private$..name <- name

      invisible(self)
    },

    getReceiver = function() private$..corpusStudio,
    getInputClass = function() return('FileSet'),
    getOutputClass = function() return('Corpus'),

    execute = function(x) {

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('FileSet', 'corpus',
                                               'VCorpus', 'SimpleCorpus',
                                               'character'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log( method = 'execute', event = v$msg, level = 'Error')
        stop()
      }

      corpus <- private$..corpusStudio$build(x, name = private$..name)
      return(corpus)
    }
  )
)
