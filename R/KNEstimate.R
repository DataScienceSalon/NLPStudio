#==============================================================================#
#                               KNEstimate                                     #
#==============================================================================#
#' KNEstimate
#'
#' \code{KNEstimate} Computes Modified Kneser Ney estimates
#'
#' Computes Modified Kneser Ney estimates for all nGrams in the Model
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family LMStudio Classes
#' @export
KNEstimate <- R6::R6Class(
  classname = "KNEstimate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNStudio0,

  private = list(

    ..nGrams = list(),
    ..discounts = data.table(),
    ..totals = data.table(),
    ..modelSize = numeric(),

    alpha = function() {

      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$alpha <- private$..nGrams[[i]]$cKN_nGram /
            private$..totals$n[i+1]

        } else if (i < private$..modelSize) {
          private$..nGrams[[i]]$alpha <-
            pmax(private$..nGrams[[i]]$cKN_nGram - private$..discounts[i],0) /
            private$..totals$n[i+1]

        } else {
          private$..nGrams[[i]]$alpha <-
            pmax(private$..nGrams[[i]]$cNGram - private$..discounts[i],0) /
            private$..nGrams[[i]]$cPre
        }
      }
    },

    lambda = function() {
      for (i in 2:private$..modelSize) {

        if (i < private$..modelSize) {
          private$..nGrams[[i]]$lambda <-
            private$..discounts[i] /
            private$..totals$n[i+1] *
            private$..nGrams[[i]]$N1pPre_
        } else {
          private$..nGrams[[i]]$lambda <-
            private$..discounts[i] /
            private$..nGrams[[i]]$cPre *
            private$..nGrams[[i]]$N1pPre_
        }
      }
    },

    pKN = function() {
      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$pKN <- private$..nGrams[[i]]$alpha

        } else {
          lower <- private$..nGrams[[i-1]][,.(nGram, pKN)]
          setnames(lower, "pKN", "pKNSuffix")
          setkey(lower, nGram)
          setkey(private$..nGrams[[i]], suffix)
          private$..nGrams[[i]] <-
            merge(private$..nGrams[[i]], lower, by.x = 'suffix',
                  by.y = 'nGram', all.x = TRUE)
          for (j in seq_along(private$..nGrams[[i]])) {
            set(private$..nGrams[[i]],
                i=which(is.na(private$..nGrams[[i]][[j]])), j=j, value=0)
          }
          private$..nGrams[[i]]$pKN <-
            private$..nGrams[[i]]$alpha +
            private$..nGrams[[i]]$lambda *
            private$..nGrams[[i]]$pKNSuffix
        }
      }
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadServices()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('KN'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..model <- x
      private$..nGrams <- x$getNGrams()
      private$..discounts <- x$getDiscounts()
      private$..totals <- x$getTotals()
      private$..modelSize <- x$getModelSize()

      event <-  paste0("Instantiated KNEstimate ")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    build = function() {

      private$alpha()
      private$lambda()
      private$pKN()

      private$..model$setNGrams(private$..nGrams)

      # Remove temporary members
      private$..nGrams <- NULL
      private$..discounts <- NULL
      private$..totals <- NULL
      private$..modelSize <- NULL

      return(private$..model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knEstimate(self)
    }
  )
)
