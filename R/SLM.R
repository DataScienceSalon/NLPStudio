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
  inherit = Super,

  private = list(
    ..evaluation = character(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()

      vocabulary <- 'Closed'
      if (private$..config$isOpen()) vocabulary <- 'Open'

      NLPStudio::printHeading(text = paste0(private$..smoothing, ": ",
                                            private$modelName, " Summary"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nId         : ", meta$identity$id))
      cat(paste0("\nName       : ", meta$identity$name))
      cat(paste0("\nType       : ", meta$functional$modelType))
      cat(paste0("\nSmoothing  : ", meta$functional$algorithm))
      cat(paste0("\nVocabulary : ", vocabulary))
      return(TRUE)

    },

    nGramSummary = function() {

      name <- self$getName()
      algorithm <- private$..config$getAlgorithm()
      modelType <- private$..config$getModelType()
      heading <- paste(name, algorithm, modelType, "nGram Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$totals)
      return(TRUE)

    },

    discountSummary = function() {

      name <- self$getName()
      algorithm <- private$..config$getAlgorithm()
      modelType <- private$..config$getModelType()
      heading <- paste(name, algorithm, modelType, "Discount Summary")

      NLPStudio::printHeading(text = heading, symbol = "-", newlines = 2)
      print(private$..model$discounts)
      return(TRUE)

    },

    nGramDetail = function() {

      name <- self$getName()
      algorithm <- private$..config$getAlgorithm()
      modelType <- private$..config$getModelType()
      heading <- paste(name, algorithm, modelType, "nGram Detail")


      if (length(private$..model$nGrams) > 0) {

        for (i in 1:private$meta$get(key = 'modelSize')) {
          NLPStudio::printHeading(text = heading,
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
    },

    validateParams = function(x, train, test, name, smoothing, modelSize,
                              openVocabulary) {
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

        if (!is.null(train) | !is.null(test)) {
          event <- paste0("When the 'x' parameter is provided and is a valid ",
                          "CVSet object, the 'train', and 'test' parameters ",
                          "are ignored.")
          private$logR$log(method = 'initialize', event = event, level = "Warn")
        }
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
      return(TRUE)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                                Constructor                              #
    #-------------------------------------------------------------------------#
    initialize = function(x = NULL, train = NULL, test = NULL, name = NULL,
                          smoothing = 'kn', modelSize = 3,
                          openVocabulary = TRUE) {

      private$loadServices(name)

      private$validateParams(x, train, test, name, smoothing, modelSize,
                             openVocabulary)

      # Create Configuration Object
      private$..config <- SLMConfig$new(smoothing = smoothing,
                                        modelSize = modelSize,
                                        name = name,
                                        openVocabulary = openVocabulary)

      # Update meta data
      private$meta$set(key = 'smoothing', value = smoothing, type = 'f')
      private$meta$set(key = 'algorithm',
                       value = private$..config$getAlgorithm(), type = 'f')
      private$meta$set(key = 'openVocabulary', value = openVocabulary, type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')

      private$meta$set(key = 'modelType',
                       value = private$..config$getModelType(), type = 'f')

      # Obtain training and test Corpus objects from x, the CVSEt
      # parameter if it is not null.
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
    evaluate = function() {
      test <- private$..corpora$getTest()
      private$..evaluation <- SLMEvaluate$new(private$..config, private$..model,
                                         test)$evaluate()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                     Accessor and Setter Methods                         #
    #-------------------------------------------------------------------------#
    getConfig = function() private$..config,
    getCorpora = function() private$..corpora,
    getModel = function() private$..model,
    getNGrams = function() private$..model$nGrams,
    setNGrams = function(x) private$..model$nGrams <- x,
    getDiscounts = function() private$..model$discounts,
    getTotals = function() private$..model$totals,
    getScores = function() private$..model$scores,
    setScores = function(x) private$..model$scores <- x,
    getEval = function() private$..model$evaluation,
    setEval = function(x) private$..model$evaluation <- x,

    #-------------------------------------------------------------------------#
    #                             Summary Method                              #
    #-------------------------------------------------------------------------#
    summary = function() {
      private$overview()
      if (length(private$..corpora) > 0) private$..corpora$summary()
      if (length(private$..model) > 0) private$..model$summary()
      if (length(private$..evaluation) > 0) private$..evaluation$summary()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$slm(self)
    }

  )
)
