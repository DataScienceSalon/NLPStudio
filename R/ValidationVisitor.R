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
#' @param object The object in its current admin
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
    document = function(object) {
      return(private$validate(object))
    },

    file = function(object) {
      return(private$validate(object))
    },

    fileSet = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                      File and Corpus Sourcing Classes                   #
    #-------------------------------------------------------------------------#
    fcsourceDir = function(object) {
      return(private$validate(object))
    },

    csourceDir = function(object) {
      return(private$validate(object))
    },

    csourceFileSet = function(object) {
      return(private$validate(object))
    },

    csourceQuanteda = function(object) {
      return(private$validate(object))
    },

    csourceTM = function(object) {
      return(private$validate(object))
    },

    csourceVector = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                     Validate Metadata Classes                           #
    #-------------------------------------------------------------------------#

    meta = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                 Validate Data Processing Classes                        #
    #-------------------------------------------------------------------------#
    tokens = function(object) {
      return(private$validate(object))
    },

    tokensSet = function(object) {
      return(private$validate(object))
    },

    pos = function(object) {
      return(private$validate(object))
    },

    posSet = function(object) {
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
    fileStudio0 = function(object) {
      return(private$validate(object))
    },

    fileStudio = function(object) {
      return(private$validate(object))
    },

    textStudio0 = function(object) {
      return(private$validate(object))
    },

    textStudio = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                             Misc Classes                                #
    #-------------------------------------------------------------------------#
    koln = function(object) {
      return(private$validate(object))
    }
  )
)
