#==============================================================================#
#                               KNEstimate                                    #
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

    alpha = function() {

      for (i in 1:private$..size) {

        if (i == 1) {
          private$..nGrams[[i]]$alpha <- private$..nGrams[[i]]$cKN /
            private$..totals$types[2]

        } else if (i < private$..size) {
          private$..nGrams[[i]]$alpha <-
            max(private$..nGrams[[i]]$cKN - private$..discount,0) /
            private$..totals$totalCkn[i+1]
        } else {
          private$..nGrams[[i]]$alpha <-
            max(private$..nGrams[[i]]$count - private$..discount,0) /
            private$..totals$totalCkn[i+1]
        }


        # Obtain nGram, prefix, continuation counts as well as discount for
        # the nGram level
        current <- private$..nGrams[[i]][,. (nGram, prefix, cKN)]
        discount <- private$..discounts[[i,2]]

        # For the unigram level alpha is the ratio of the continuation unigram
        # count and the number of bigram types.
        if (i == 1) {
          nBigrams <- nrow(private$..nGrams[[i+1]])
          current[,.(alpha =  ifelse(cKN - discount > 0,
                                     cKN - discount, 0)
                     / nBigrams)]

        # For the higher levels, alpha is computed as described above
        } else {
          prefix <- private$..nGrams[[i-1]][,.(nGram, cKN)]
          setnames(prefix, "cKN", "cKNContext")
          current <- merge(current, prefix, by.x = 'prefix',
                           by.y = 'nGram', all.x = TRUE)
          for (k in seq_along(current)) {
            set(current, i=which(is.na(current[[k]])), j=k, value=0)
          }
          current[,.(alpha =  ifelse(cKN - discount > 0,
                                     cKN - discount, 0)
                     / cKNContext)]
        }
        current <- current[,.(nGram, alpha, cKNContext)]
        private$..nGrams[[i]] <- merge(private$..nGrams[[i]],
                                              current,  by = 'nGram',
                                              all.x = TRUE)
      }
    },

    lambda = function() {

      for (i in 1:private$..size) {

        current <- private$..nGrams[[i]][,. (nGram, cKN, cknContext)]

        if (i > 1) {

        }

        if (i < private$..size) {
          higher <- private$..nGrams[[i+1]][,.(prefix)]
          counts <- higher[,.(cKN = .N), by = .(prefix)]

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
      private$..params$classes$valid <- list(c('KN', 'KN', 'Katz', 'SBO'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..lm <- x
      private$..nGrams <- x$getnGrams()
      private$..discount <- x$getDiscounts()
      private$..totals <- x$getTotals()
      private$..size <- x$getSize()
      invisible(self)
    },

    build = function() {

      private$alpha()
      private$lambda()
      private$pKN()

      private$..lm$setTables(private$..nGrams)

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
