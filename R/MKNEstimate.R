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
#' @docTypes class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNEstimate <- R6::R6Class(
  classname = "MKNEstimate",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNEstimate,

  private = list(

    alpha = function() {

      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$alpha <- private$..nGrams[[i]]$cMKN_nGram /
            private$..totals$n[i+1]

        } else {

          # Select discounts for nGram Order
          discounts <- t(private$..discounts[i,-c(1,2)])

          # Compute discount group based upon continuation counts
          private$..nGrams[[i]] <-
            private$..nGrams[[i]][, cMKN_Group := pmin(3, (cMKN_nGram)),
                                  by=cMKN_nGram]

          # Determine the discount based upon the count group
          private$..nGrams[[i]] <-
            private$..nGrams[[i]][, D := discounts[cMKN_Group]]


          if (i < private$..modelSize) {
            private$..nGrams[[i]][, alpha := (cMKN_nGram - D)]
            private$..nGrams[[i]]$alpha <- pmax(private$..nGrams[[i]]$alpha, 0) /
              as.numeric(private$..totals$n[i+1])

          } else {
            # Compute highest order pseudo probability alpha
            private$..nGrams[[i]][, alpha := (cMKN_nGram - D)]
            private$..nGrams[[i]]$alpha <- pmax(private$..nGrams[[i]]$alpha, 0) /
              as.numeric(private$..nGrams[[i]]$cPre)
          }
        }
      }
    },

    lambda = function() {
      for (i in 2:private$..modelSize) {

        D1 <- private$..discounts$D1[i]
        D2 <- private$..discounts$D2[i]
        D3 <- private$..discounts$D3[i]

        discounts <-
          D1 * private$..nGrams[[i]]$N1Pre_ +
          D2 * private$..nGrams[[i]]$N2Pre_ +
          D3 * private$..nGrams[[i]]$N3pPre_

        if (i < private$..modelSize) {
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
      for (i in 1:private$..modelSize) {

        if (i == 1) {
          private$..nGrams[[i]]$pMKN <- private$..nGrams[[i]]$alpha

        } else {
          lower <- private$..nGrams[[i-1]][,.(nGram, pMKN)]
          setnames(lower, "pMKN", "pMKNSuffix")
          setkey(lower, nGram)
          setkey(private$..nGrams[[i]], suffix)
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
      private$..params$classes$valid <- list(c('MKN'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }


      # Dock current lm (extract members read/updated within class)
      private$..model <- x
      private$..nGrams <- x$getNGrams()
      private$..discounts <- x$getDiscounts()
      private$..totals <- x$getTotals()
      private$..modelSize <- x$getModelSize()

      event <-  paste0("Instantiated MKNEstimate ")
      private$logR$log(method = 'initialize', event = event)

      invisible(self)
    },

    build = function() {

      private$alpha()
      private$lambda()
      private$pMKN()


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
      visitor$mknEstimate(self)
    }
  )
)
