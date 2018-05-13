#==============================================================================#
#                               MKNCounts                                      #
#==============================================================================#
#' MKNCounts
#'
#' \code{MKNCounts} Computes counts used to estimate Modified Kneser Ney probabilities
#'
#' Computes counts used in the probability calculations for the Modified Kneser Ney
#' probabilities.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family MKNStudio Classes
#' @family LMStudio Classes
#' @export
MKNCounts <- R6::R6Class(
  classname = "MKNCounts",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = MKNStudio0,

  private = list(

    discounts = function() {
      private$..discounts <- rbindlist(lapply(seq_along(1:private$..size), function(i) {
        d <- list()
        d$D <- private$..totals$n1[i] /
          (private$..totals$n1[i] + 2 * private$..totals$n2[i])

        d$D0 <- 0

        d$D1 <- 1 - (2 * d$D *
                                       private$..totals$n2[i] /
                                       private$..totals$n1[i])

        d$D2 <- 2 - (3 * d$D *
                                       private$..totals$n3[i] /
                                       private$..totals$n2[i])

        d$D3 <- 3 - (4 * d$D *
                                       private$..totals$n4[i] /
                                       private$..totals$n3[i])
        d
      }))
      return(TRUE)
    },

    hist = function(n) {
      # Compute the number of histories in which the prefix appears
      # precisely 1, 2 and 3 or more times.

      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N1 := sum(unique(cNGram == 1)),  by = .(prefix)]
      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N2 := sum(unique(cNGram == 2)),  by = .(prefix)]
      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N3 := sum(unique(cNGram > 2)),  by = .(prefix)]
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
      private$..lm <- x
      private$..document <- x$getDocument()
      private$..size <- x$getSize()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknCounts(self)
    }
  )
)
