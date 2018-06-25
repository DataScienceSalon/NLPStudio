#==============================================================================#
#                                    KN                                        #
#==============================================================================#
#' KN
#'
#' \code{KN} Class containing the Kneser Ney Language Model object.
#'
#' @param x CVSet object containing a training and test set.
#' @param modelSize Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#' @param name Character string. Represents name to be assigned to the KN object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @family Kneser-Ney Language Model Classes
#' @export
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..modelName = character(),
    ..modelSize = numeric(),
    ..smoothing = character(),
    ..modelType = character(),
    ..modelTypes = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),
    ..openVocabulary = logical(),
    ..nGrams = list(),
    ..discounts = data.table(),
    ..totals = data.table(),
    ..scores = data.table(),
    ..evaluation = data.table(),

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
      cat(paste0("\nType       : ", meta$functional$modelType))
      cat(paste0("\nSmoothing  : ", meta$functional$smoothing))
      cat(paste0("\nVocabulary : ", vocabulary))
      return(TRUE)

    },

    nGramSummary = function() {

      NLPStudio::printHeading(text = 'nGram Summary', symbol = "-", newlines = 2)
      print(private$..totals)
      return(TRUE)

    },

    discountSummary = function() {

      NLPStudio::printHeading(text = 'Discounts', symbol = "-", newlines = 2)
      print(private$..discounts)
      return(TRUE)

    },

    nGramDetail = function() {

      if (length(private$..nGrams) > 0) {

        for (i in 1:private$meta$get(key = 'modelSize')) {
          NLPStudio::printHeading(text = paste0(private$..smoothing, ": ",
                                                private$..modelType[i], " Summary"),
                                  symbol = "-",
                                  newlines = 2)
          setkey(private$..nGrams[[i]], nGram)
          print(private$..nGrams[[i]][, tail(.SD, 10), by=nGram])
        }
      }
      return(TRUE)
    },

    evaluation = function() {

      NLPStudio::printHeading(text = 'Evaluation', symbol = "-", newlines = 2)
      print(private$..evaluation)
      return(TRUE)

    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, modelSize = 3, open = TRUE, name = NULL) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list('CVSet')
      private$..params$range$variable <- c('modelSize')
      private$..params$range$value <- c(modelSize)
      private$..params$range$low <- 1
      private$..params$range$high <- 5
      private$..params$logicals$variables <- c('open')
      private$..params$logicals$values <- c(open)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Unpack the Corpora object
      train <- corpora$getTrain()
      test <- corpora$getTest()

      if (is.null(name)) {
        name <- train$getName()
      }
      private$meta <- Meta$new(x = self, name = name)

      # Initalize member variables
      private$..modelName <- name
      private$..train <- train
      private$..test <- test
      private$..smoothing <- 'Kneser-Ney'
      private$..modelSize <- modelSize
      private$..modelType <- private$..modelTypes[modelSize]
      private$..openVocabulary <- open

      # Initialize private members
      private$meta$set(key = 'smoothing', value = "Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = modelSize, type = 'f')
      private$meta$set(key = 'modelType', value = private$..modelTypes[modelSize], type = 'f')
      private$meta$set(key = 'openVocabulary', value = open, type = 'f')

      # Create log entry
      event <- paste0("KN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    isOpen = function() private$..openVocabulary,

    getName = function() private$..modelName,
    getId = function() private$meta$get(key = 'id'),
    getModelSize = function() private$..modelSize,
    getModelTypes = function() private$..modelTypes,

    getTrain = function() private$..train,
    getTest = function() private$..test,

    getNGrams = function() private$..nGrams,
    setNGrams = function(nGrams)  private$..nGrams <- nGrams,

    getDiscounts = function() private$..discounts,
    setDiscounts = function(discounts)  private$..discounts <- discounts,

    getTotals = function() private$..totals,
    setTotals = function(totals)  private$..totals <- totals,

    getScores = function() private$..scores,
    setScores = function(scores)  private$..scores <- scores,

    getEval = function() private$..evaluation,
    setEval = function(eval) private$..evaluation <- eval,

    summary = function() {
      private$overview()
      private$..train$summary(section = c("i", "q"))
      private$..test$summary(section = c("i", "q"))
      private$discountSummary()
      private$nGramSummary()
      #private$nGramDetail()
      private$evaluation()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }

  )
)
