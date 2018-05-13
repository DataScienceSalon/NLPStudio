#==============================================================================#
#                               MKNEstimate                                    #
#==============================================================================#
#' MKNEstimate
#'
#' \code{MKNEstimate} Computes Modified Kneser Ney estimates
#'
#' Computes Modified Kneser Ney estimates for all nGrams in the Model
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNEstimate <- R6::R6Class(
  classname = "MKNEstimate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = MKNStudio0,

  private = list(

    alpha = function() {

      for (i in 1:private$..size) {

        if (i == 1) {
          private$..nGrams[[i]]$alpha <- private$..nGrams[[i]]$cMKN_nGram /
            private$..totals$n[i+1]

        } else {
          discount <- private$..discounts[i,pmin(private$..nGrams[[i]]$cMKN_nGram,3)+2]
          if (i < private$..size) {
          private$..nGrams[[i]]$alpha <-
            pmax(private$..nGrams[[i]]$cMKN_nGram - discount, 0) /
            private$..totals$n[i+1]

          } else {
            private$..nGrams[[i]]$alpha <-
              pmax(private$..nGrams[[i]]$cNGram - discount, 0) /
              private$..nGrams[[i]]$cPre
          }
        }
      }
    },

    lambda = function() {
      for (i in 2:private$..size) {

        D1 <- private$..discounts$D1[i]
        D2 <- private$..discounts$D2[i]
        D3 <- private$..discounts$D3[i]

        discounts <-
          D1 * private$..nGrams[[i]]$N1Pre_ +
          D2 * private$..nGrams[[i]]$N2Pre_ +
          D3 * private$..nGrams[[i]]$N3pPre_ +

        if (i < private$..size) {
          private$..nGrams[[i]]$lambda <-
            discounts /
            private$..totals$n[i+1]
        } else {
          private$..nGrams[[i]]$lambda <-
            discounts /
            private$..nGrams[[i]]$cPre
        }
      }
    },

    pMKN = function() {
      for (i in 1:private$..size) {

        if (i == 1) {
          private$..nGrams[[i]]$pMKN <- private$..nGrams[[i]]$alpha

        } else {
          lower <- private$..nGrams[[i-1]][,.(nGram, pMKN)]
          setnames(lower, "pMKN", "pMKNSuffix")
          private$..nGrams[[i]] <-
            merge(private$..nGrams[[i]], lower, by.x = 'suffix',
                  by.y = 'nGram', all.x = TRUE)
          for (j in seq_along(private$..nGrams[[i]])) {
            set(private$..nGrams[[i]],
                i=which(is.na(private$..nGrams[[i]][[j]])), j=j, value=0)
          }
          private$..nGrams[[i]]$pMKN <-
            private$..nGrams[[i]]$alpha +
            private$..nGrams[[i]]$lambda *
            private$..nGrams[[i]]$pMKNSuffix
        }
      }
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(x) {

      private$loadDependencies()

      # Validation
      private$..params <- list()
      private$..params$classes$name <- list('x')
      private$..params$classes$objects <- list(x)
      private$..params$classes$valid <- list(c('MKN', 'MKN', 'Katz', 'SBO'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..lm <- x
      private$..nGrams <- x$getnGrams()
      private$..discounts <- x$getDiscounts()
      private$..totals <- x$getTotals()
      private$..size <- x$getSize()
      invisible(self)
    },

    build = function() {

      private$alpha()
      private$lambda()
      private$pMKN()

      private$..lm$setnGrams(private$..nGrams)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$knEstimate(self)
    }
  )
)
