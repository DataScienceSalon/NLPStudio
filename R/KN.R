#==============================================================================#
#                                   KN                                         #
#==============================================================================#
#' KN
#'
#' \code{KN} Kneser Ney language model object.
#'
#' Kneser Ney language model object.
#'
#' @param x Corpus object upon which the model will be trained.
#' @param size Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @export
KN <- R6::R6Class(
  classname = "KN",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    ..x = character(),
    ..document = character(),
    ..modelType = c('Unigram', 'Bigram', 'Trigram', 'Quadgram', 'Quintgram'),
    ..nGrams = list(),
    ..discounts = numeric(),

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    overview = function() {

      meta <- private$meta$get()
      if (self$is.openVocabulary())  {
        vocabulary <- 'Open'
      } else {
        vocabulary <- 'Closed'
      }
      NLPStudio::printHeading(text = paste0(meta$functional$smoothing, ": ",
                                            self$getName(), " Summary"),
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

    nGramDetail = function() {

      for (i in 1:private$meta$get(key = 'modelSize')) {
        meta <- private$meta$get()
        NLPStudio::printHeading(text = paste0(meta$functional$smoothing, ": ",
                                              private$..modelType[i], " Summary"),
                                symbol = "-",
                                newlines = 2)
        print(private$..nGrams[[i]][, tail(.SD, 10), by=cKN_nGram])
      }
      return(TRUE)
    }
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(x, size = 3, open = TRUE, name = NULL) {

      if (is.null(name)) {
        name <- x$getName()
        name <- paste0(name, " Kneser-Ney Model")
      }

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('Corpus'))
      private$..params$logicals$variables <- list("open")
      private$..params$logicals$values <- list(open)
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      } else if (size < 1 | size > 5) {
        event <- "Model size must be between 1 and 5."
        private$logR$log(method = 'initialize',
                         event = event, level = "Error")
        stop()
      }

      # Initialize private members
      private$..x <- x
      private$meta$set(key = 'smoothing', value = "Kneser-Ney", type = 'f')
      private$meta$set(key = 'modelSize', value = size, type = 'f')
      private$meta$set(key = 'modelType', value = private$..modelType[size], type = 'f')
      private$meta$set(key = 'openVocabulary', value = open, type = 'f')

      # Create log entry
      event <- paste0("KN Language Model Object Instantiated.")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                           Setters / Getters                             #
    #-------------------------------------------------------------------------#
    getId = function() { return(private$meta$get(key = 'id'))},

    getName = function() { return(private$meta$get(key = 'name'))},

    getCorpus = function() { return(private$..x) },

    getType = function() { return(private$meta$get(key = 'modelType')) },

    getSmoothing = function() { return(private$meta$get(key = 'smoothing')) },

    getSize = function() { return(private$meta$get(key = 'modelSize')) },

    is.openVocabulary = function() { return(private$meta$get(key = 'openVocabulary')) },

    getDocument = function() { return(private$..document) },
    setDocument = function(x) {
      private$..document <- x
      invisible(self)
    },

    getnGrams = function() { return(private$..nGrams) },
    setnGrams = function(x) {
      private$..nGrams <- x
      invisible(self)
    },

    getDiscounts = function() { return(private$..discounts) },
    setDiscounts = function(x) {
      private$..discounts <- x
      invisible(self)
    },

    getTotals = function() { return(private$..totals) },
    setTotals = function(x) {
      private$..totals <- x
      invisible(self)
    },

    summary = function() {
      private$overview()
      private$..document$summary(section = c("i", "q"))
      private$nGramSummary()
      private$nGramDetail()
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$kn(self)
    }

  )
)
