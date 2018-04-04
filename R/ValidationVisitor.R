#' ValidationVisitor
#'
#' \code{ValidationVisitor} Visitor class responsible for validation
#'
#' \strong{ValidationVisitor Methods:}
#' The ValidationVisitor methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the instantiation of the NLPStudio object}
#' }
#'
#' @param object The object in its current state
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
ValidationVisitor <- R6::R6Class(
  classname = "ValidationVisitor",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object) {
      status <- list()
      status[['code']] <- TRUE

      p <- object$getParams()

      if (!is.null(p$classes)) {
        status <- validateClass(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate pattern replace
      if (!is.null(p$kv)) {
        status <- validateKeyValue(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate Logicals
      if (!is.null(p$logicals)) {
        status <- validateLogical(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate discrete parameters
      if (!is.null(p$discrete)) {
        status <- validateDiscrete(object)
        if (status$code == FALSE) {
          return(status)
        }
      }
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Validate Core Classes                            #
    #-------------------------------------------------------------------------#
    nlpStudio = function(object) {
      return(private$validate(object))
    },
    corpus = function(object) {
      return(private$validate(object))

    },
    textDocument = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                 Validate Data Processing Classes                        #
    #-------------------------------------------------------------------------#
    tokenFactorySent = function(object) {
      return(private$validate(object))
    },

    tokenFactoryWord = function(object) {
      return(private$validate(object))
    },

    tokens = function(object) {
      return(private$validate(object))
    },

    posFactory = function(object) {
      return(private$validate(object))
    },

    pos = function(object) {
      return(private$validate(object))
    },

    termFreq = function(object) {
      return(private$validate(object))
    },

    termFreqFactoryDfm = function(object) {
      return(private$validate(object))
    },

    termFreqFactoryDtm = function(object) {
      return(private$validate(object))
    },

    termFreqFactoryTdm = function(object) {
      return(private$validate(object))
    },

    termFreqDfm = function(object) {
      return(private$validate(object))
    },

    termFreqDtm = function(object) {
      return(private$validate(object))
    },

    termFreqTdm = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                             Studio Classes                              #
    #-------------------------------------------------------------------------#
    textStudio = function(object) {
      return(private$validate(object))
    },

    dataStudio = function(object) {
      return(private$validate(object))
    },


    #-------------------------------------------------------------------------#
    #                             Misc Classes                                #
    #-------------------------------------------------------------------------#
    cloneCorpus = function(object) {
      return(private$validate(object))
    },

    cloneDocument = function(object) {
      return(private$validate(object))
    }
  )
)
