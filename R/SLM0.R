#==============================================================================#
#                                  SLM0                                        #
#==============================================================================#
#' SLM0
#'
#' \code{SLM0} Base class for the Statistical Language Model Studio (SLM) family of classes.
#'
#' This base class defines common members for the SLM family of classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @family Statistical Language Model Classes
#' @export
SLM0 <- R6::R6Class(
  classname = "SLM0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..config = character(),
    ..corpora = character(),
    ..model = character(),
    ..evaluation = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()

      vocabulary <- 'Closed'
      if (private$..private$..config$isOpen()) vocabulary <- 'Open'

      NLPStudio::printHeading(text = paste0(private$..smoothing, ": ",
                                            private$modelName, " Summary"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nId         : ", meta$identity$id))
      cat(paste0("\nName       : ", meta$identity$name))
      cat(paste0("\nType       : ", meta$functional$modelType))
      cat(paste0("\nSmoothing  : ", meta$functional$smoothing))
      cat(paste0("\nVocabulary : ", vocabulary))
      return(TRUE)

    },

    nGramSummary = function() {

      NLPStudio::printHeading(text = 'nGram Summary', symbol = "-", newlines = 2)
      print(private$..model$totals)
      return(TRUE)

    },

    discountSummary = function() {

      NLPStudio::printHeading(text = 'Discounts', symbol = "-", newlines = 2)
      print(private$..model$discounts)
      return(TRUE)

    },

    nGramDetail = function() {

      smoothing <- private$..config$getSmoothing()
      modelTypes <- private$..config$getModelTypes()

      if (length(private$..model$nGrams) > 0) {

        for (i in 1:private$meta$get(key = 'modelSize')) {
          NLPStudio::printHeading(text = paste0(smoothing, ": ",
                                                modelType[i], " Summary"),
                                  symbol = "-",
                                  newlines = 2)
          setkey(private$..model$nGrams[[i]], nGram)
          print(private$..model$nGrams[[i]][, tail(.SD, 10), by=nGram])
        }
      }
      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Validation Methods                            #
    #-------------------------------------------------------------------------#
    validateClass = function(x, fieldName, className, methodName) {

      private$..params <- list()
      private$..params$classes$name <- list(fieldName)
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(className)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = methodName, event = v$msg, level = "Error")
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  ),

  public = list(
    initialize = function(x) { stop("Not implemented for this abstract/interface class.") },
    getConfig = function() private$..config,
    getCorpora = function() private$..corpora,
    getModel = function() private$..model,
    getEval = function() private$..evaluation

  )
)
