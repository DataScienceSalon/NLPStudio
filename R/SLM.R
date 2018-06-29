#==============================================================================#
#                                   SLM                                        #
#==============================================================================#
#' SLM
#'
#' \code{SLM} Statistical language model class.
#'
#' Encapsulates a Statistical Language Model implementing one of several
#' smoothing algorithsm, such as Katz, Kneser-Ney, Modified Kneser-Ney,
#' and Stupid-Backoff.
#'
#' @param x a CVSet containing training and test Corpus objects
#' @param train Train Corpus object. Ignored if x is a CVSet, required otherwise.
#' @param test Test Corpus object. Ignored if x is a CVSet, required otherwise.
#' @param smooth Smoothing algorithm. Valid options include:
#'  'katz': Katz Moothing
#'  'kn': Kneser-Ney
#'  'mkn': Modified Kneser-Ney
#'  'sbo': Stupid Backoff
#' @param modelSize Numeric between 1, for unigram to 5 for quintgram.
#' @param openVocabulary Logical. If TRUE, preprocessing will replace all words
#' in the test corpus that are not in the training corpus with the pseudo-word
#' UNK. If FALSE, all words in test corpus are assumed to be in the training
#' corpus. The default is TRUE.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Statistical Language Model Classes
#' @export
SLM <- R6::R6Class(
  classname = "SLM",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = SLM0,

  private = list(
    ..evaluation = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()

      vocabulary <- 'Closed'
      if (private$..openVocabulary) vocabulary <- 'Open'

      NLPStudio::printHeading(text = paste0(private$..smoothing, ": ",
                                            private$modelName, " Summary"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nId         : ", meta$identity$id))
      cat(paste0("\nName       : ", meta$identity$name))
      cat(paste0("\nType       : ", private$..settings$modelType))
      cat(paste0("\nSmoothing  : ", private$..settings$smoothing))
      cat(paste0("\nVocabulary : ", vocabulary))
      return(TRUE)

    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function(x = NULL, train = NULL, test = NULL, name = NULL, smoothing = 'kn',
                          modelSize = 3, openVocabulary = TRUE) {

      private$loadServices(name)

      #-----------------------------------------------------------------------#
      #                               Validation                              #
      #-----------------------------------------------------------------------#
      # Validate CVSet and Training/Test Set Parameters
      if (is.null(x) & is.null(train)) {
        event <- paste0("Either the 'x' parameter, a CVSEt object, or the ",
                        "'train' parameter, a Corpus object, must be provided. ",
                        "See ?", class(self)[1], " for further assistance.")
        private$logR$log(method = 'initialize', event = event, level = "Error")
        stop()
      } else if (is.null(x) & is.null(test)) {
        event <- paste0("Either the 'x' parameter, a CVSEt object, or the ",
                        "'test' parameter, a Corpus object, must be provided. ",
                        "See ?", class(self)[1], " for further assistance.")
        private$logR$log(method = 'initialize', event = event, level = "Error")
        stop()
      }

      if (is.null(x)) {
        if (private$validateClass(train, fieldName = 'train',
                                  className = 'Corpus',
                                  methodName = 'initialize') == FALSE) stop()
        if (private$validateClass(test, fieldName = 'test',
                                  className = 'Corpus',
                                  methodName = 'initialize') == FALSE) stop()
      } else {
        if (private$validateClass(x, fieldName = 'x',
                                 className = 'CVSet',
                                 methodName = 'initialize') == FALSE) stop()
      }

      # Validate other parameters
      private$..params$discrete$variables = list('smoothing')
      private$..params$discrete$values = list(smoothing)
      private$..params$discrete$valid = list(c('katz', 'kn', 'mkn', 'sbo'))
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      private$..params$logicals$variables <- c('openVocabulary')
      private$..params$logicals$values <- c(openVocabulary)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      # Create Configuration Object
      private$..config <- SLMConfig$new(smoothing = smoothing,
                                        modelSize = modelSize,
                                        name = name,
                                        openVocabulary = openVocabulary)

      # Obtain training and test Corpus objects from x (CVSet) parameter.
      if (!is.null(x)) {
        train <- x$getTrain()
        test <- x$getTest()
      }

      # Create Training and Test Corpora for Modeling
      private$..corpora <- SLMCorpora$new(train, test, private$..config)$build()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                                Build Model                              #
    #-------------------------------------------------------------------------#
    fit = function() {

      smoothing <- private$..config$getSmoothing()
      private$..model <- switch(smoothing,
                                katz = KatzStudio$new(private$..config,
                                                      private$..corpora)$build()$getModel(),
                                kn = KNStudio$new(private$..config,
                                                  private$..corpora)$build()$getModel(),
                                mkn = MKNStudio$new(private$..config,
                                                    private$..corpora)$build()$getModel(),
                                sbo = SBOStudio$new(private$..config,
                                                    private$..corpora)$build()$getModel()
                                )
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Evaluate Model                              #
    #-------------------------------------------------------------------------#
    evaluateModel = function() {
      test <- private$..corpora$getTest()
      private$..model <- SLMEvaluate$new(private$..config, private$..model,
                                         test)$evaluate()$getModel()
      private$..evaluation <- private$..model$getEval()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Accessor Methods                            #
    #-------------------------------------------------------------------------#
    getConfig = function() private$..config,
    getCorpora = function() private$..corpora,
    getModel = function() private$..model,
    getEvaluation = function() private$..evaluation,

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$slm(self)
    }

  )
)
