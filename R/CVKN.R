#' CVKN
#'
#' \code{CVKN} Class containing training, test and an optional validation corpora objects.
#'
#' Class conitains a cross-validation set of corpora objects. CVKN objects are
#' collections including a training, test, and an optional validation Corpus objects.
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the CVKN class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the CVKN object.}
#'  }
#'
#' @param name Character string containing the name for the CVKN object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the CVKNs
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return CVKN object, containing the CVKN text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CVKN Classes
#' @export
CVKN <- R6::R6Class(
  classname = "CVKN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CV,

  private = list(
    ..x = character(),
    ..cvSets = list(),
    ..cv = numeric(),
    ..modelSize = numeric(),
    ..epsilon = numeric(),
    ..openVocabulary = logical()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Constructor                                 #
    #-------------------------------------------------------------------------#
    initialize = function(x,  kFolds = 5, modelSize = 3, epsilon = 10^-7,
                          openVocabulary = TRUE, stratify = TRUE) {

      private$loadServices("Kneser-Ney Cross Validation")

      private$..params <- list()
      private$..params$classes$name <- list('kFolds', 'modelSize', 'epsilon',
                                            'openVocabulary')
      private$..params$classes$objects <- list(kFolds, modelSize, epsilon,
                                               openVocabulary)
      private$..params$classes$valid <- list('numeric', 'numeric', 'numeric',
                                             'logical')
      private$..params$logicals$variables <- c('stratify', 'openVocabulary')
      private$..params$logicals$values <- c(stratify, openVocabulary)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..x <- x
      private$..kFolds <- kFolds
      private$..modelSize <- modelSize
      private$..epsilon <- epsilon
      private$..stratify <- stratify
      private$..openVocabulary <- openVocabulary

      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                  fit                                    #
    #-------------------------------------------------------------------------#
    fit = function() {

      private$createCV()

      private..results <- lapply(private$..modelSize, function(size) {
        lapply(private$..epsilon, function(e) {
          lapply(private$..openVocabulary, function(v) {
            print("Processing...")
            print(paste(" modelSize:", size))
            print(paste("   epsilon:", e))
            print(paste("vocabulary:", v))
            folds <- private$..cvSets$getCVSets()
            lapply(folds, function(f) {
              KN$new(train = f$getTrain(), modelSize = size, epsilon = e,
                     openVocabulary = v)$fit()$evaluate(test = f$getTest())$getEval()
            })
          })
        })
      })
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             accessor methods                            #
    #-------------------------------------------------------------------------#
    getResult = function() private$..results,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cvKN(self)
    }
  )
)
