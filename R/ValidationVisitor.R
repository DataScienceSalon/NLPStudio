#' ValidationVisitor
#'
#' \code{ValidationVisitor} Visitor class responsible for validation
#'
#' \strong{ValidationVisitor Method :}
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

      if (!is.null(p$range)) {
        status <- validateRange(object)
        if (status$code == FALSE) {
          return(status)
        }
      }

      # Validate path
      if (!is.null(p$file)) {
        status <- validatePath(object)
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
    file = function(object) {
      return(private$validate(object))
    },
    fileSet = function(object) {
      return(private$validate(object))
    },

    fold = function(object) {
      return(private$validate(object))
    },

    corpus = function(object) {
      return(private$validate(object))
    },

    document = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                      Command Pattern Classes                            #
    #-------------------------------------------------------------------------#
    cmd = function(object) {
      return(private$validate(object))
    },

    director = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                       File Sourcing Classes                             #
    #-------------------------------------------------------------------------#
    fileSource = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Sourcing Classes                            #
    #-------------------------------------------------------------------------#
    csourceFileSet = function(object) {
      return(private$validate(object))
    },

    csourceDir = function(object) {
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
    #                      Cross-Validation Classes                           #
    #-------------------------------------------------------------------------#
    split = function(object) {
      return(private$validate(object))
    },

    sample = function(object) {
      return(private$validate(object))
    },

    segment = function(object) {
      return(private$validate(object))
    },

    splitKFold = function(object) {
      return(private$validate(object))
    },

    cvSet = function(object) {
      return(private$validate(object))
    },

    cvSetKFold = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                     Validate Metadata Classes                           #
    #-------------------------------------------------------------------------#
    meta = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                    Validate Data Factory Classes                        #
    #-------------------------------------------------------------------------#
    tokenizer = function(object) {
      return(private$validate(object))
    },

    tagger = function(object) {
      return(private$validate(object))
    },

    counter = function(object) {
      return(private$validate(object))
    },

    nGrammer = function(object) {
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

    count = function(object) {
      return(private$validate(object))
    },

    nGram = function(object) {
      return(private$validate(object))
    },

    nGramSet = function(object) {
      return(private$validate(object))
    },


    #-------------------------------------------------------------------------#
    #                             Studio Classes                              #
    #-------------------------------------------------------------------------#
    fileStudio = function(object) {
      return(private$validate(object))
    },

    corpusStudio = function(object) {
      return(private$validate(object))
    },

    textStudio0 = function(object) {
      return(private$validate(object))
    },

    textStudio = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                         Clone and Copy Classes                          #
    #-------------------------------------------------------------------------#
    koln = function(object) {
      return(private$validate(object))
    },

    copy = function(object) {
      return(private$validate(object))
    },

    #-------------------------------------------------------------------------#
    #                  Validate Language Model Classes                        #
    #-------------------------------------------------------------------------#
    kn = function(object) {
      return(private$validate(object))
    },

    mkn = function(object) {
      return(private$validate(object))
    },

    katz = function(object) {
      return(private$validate(object))
    },

    sbo = function(object) {
      return(private$validate(object))
    }
  )
)
