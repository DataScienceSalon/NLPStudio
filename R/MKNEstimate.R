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
      # Computes alpha = (max(cKN(nGram) -D,0) / cKN(nGramContext))
      #TODO: 1.Add discount level to data table.
      #TODO: 2. Update discount to compute based upon col index min(discount level,3)
      for (i in 1:private$..size) {

        # Obtain nGram, context, continuation counts as well as discount for
        # the nGram level
        current <- private$..nGrams[[i]][,. (nGram, context, cKN)]
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
          context <- private$..nGrams[[i-1]][,.(nGram, cKN)]
          setnames(context, "cKN", "cKNContext")
          current <- merge(current, context, by.x = 'context',
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
          higher <- private$..nGrams[[i+1]][,.(context)]
          counts <- higher[,.(cKN = .N), by = .(context)]

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
      private$..params$classes$valid <- list(c('MKN', 'Katz', 'SBO'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize',
                         event = v$msg, level = "Error")
        stop()
      }

      # Dock current lm (extract members read/updated within class)
      private$..lm <- x
      private$..tables <- x$getTables()
      private$..size <- x$getSize()
      invisible(self)
    },

    build = function() {

      private$alpha()

      private$lambda()

      private$pmkn()


      private$..lm$setTables(private$..tables)

      return(private$..lm)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknEstimate(self)
    }
  )
)
