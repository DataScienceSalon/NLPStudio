#==============================================================================#
#                               MKNBuild                                       #
#==============================================================================#
#' MKNBuild
#'
#' \code{MKNBuild} Computes counts used to estimate Modified Kneser Ney probabilities
#'
#' Computes counts used in the probability calculations for the
#' Modified Kneser Ney probabilities.
#'
#' @param x Language model object
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family KNStudio Classes
#' @family SLMStudio Classes
#' @export
MKNBuild <- R6::R6Class(
  classname = "MKNBuild",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KNBuild0,

  private = list(
    ..model = list(
      nGrams = list(),
      totals = data.table(),
      discounts = data.table()
    ),

    #-------------------------------------------------------------------------#
    #                           Compute Discounts                             #
    #-------------------------------------------------------------------------#
    discounts = function() {
      private$..discounts <- rbindlist(lapply(seq_along(1:private$..modelSize), function(i) {
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

    #-------------------------------------------------------------------------#
    #                         Compute Total Counts                            #
    #-------------------------------------------------------------------------#
    totals = function() {

      for (i in 1:private$..modelSize) {

        # Summarize counts and store in summary table
        nGram <- private$..modelTypes[i]
        n <- nrow(private$..nGrams[[i]])
        n1 <- nrow(private$..nGrams[[i]] %>% filter(cNGram == 1))
        n2 <- nrow(private$..nGrams[[i]] %>% filter(cNGram == 2))
        n3 <- nrow(private$..nGrams[[i]] %>% filter(cNGram == 3))
        n4 <- nrow(private$..nGrams[[i]] %>% filter(cNGram == 4))

        dt_i <- data.table(nGram = nGram, n = n, n1 = n1, n2 = n2,
                           n3 = n3, n4 = n4)
        private$..totals <- rbind(private$..totals, dt_i)
      }
      return(TRUE)
    },
    #-------------------------------------------------------------------------#
    #                       Compute Continuation Counts                       #
    #-------------------------------------------------------------------------#
    ckn = function(n) {
      # Compute continuation counts e.g. the number of words that
      # precede an nGram.  This is compute for all levels except
      # the highest.

      if (n < private$..modelSize) {

        # Compute the continuation count for the nGram
        higher <- private$..nGrams[[n+1]][,.(suffix)]
        higher <- higher[,.(cMKN_nGram = .N), by = .(suffix)]
        higher$cMKN_nGram <- as.numeric(higher$cMKN_nGram)
        private$..nGrams[[n]] <-
          merge(private$..nGrams[[n]], higher, by.x = 'nGram',
                by.y = 'suffix', all.x = TRUE)

        # Handle special case where nGram is sequence of BOS tags
        # The continuation count is the number of occurences
        # of BOS tag.
        private$..nGrams[[n]][like(nGram, "BOS"), cMKN_nGram := cNGram]

      } else {
        private$..nGrams[[n]]$cMKN_nGram <-
          as.numeric(private$..nGrams[[n]]$cNGram)
      }

      return(TRUE)
    },

    hist = function(n) {
      # Compute the number of histories in which the prefix appears
      # precisely 1, 2 and 3 or more times.

      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N1Pre_ := sum(unique(cNGram == 1)),  by = .(prefix)]
      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N2Pre_ := sum(unique(cNGram == 2)),  by = .(prefix)]
      private$..nGrams[[n]] <-
        private$..nGrams[[n]][, N3pPre_ := sum(unique(cNGram > 2)),  by = .(prefix)]
    }
  ),


  public = list(
    #-------------------------------------------------------------------------#
    #                              Constructor                                #
    #-------------------------------------------------------------------------#
    initialize = function(config, corpora) {

      private$loadServices()

      private$..params <- list()
      private$..params$classes$name <- list('config', 'corpora')
      private$..params$classes$objects <- list(config, corpora)
      private$..params$classes$valid <- list('SLMConfig','SLMCorpora')
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'initialize', event = v$msg, level = "Error")
        stop()
      }

      private$..config <- config
      private$..corpora <- corpora

      event <-  paste0("Instantiated MKNBuild ")
      private$logR$log(method = 'initialize', event = event)
      invisible(self)
    },

    getModel = function() {
      model <- MKN$new(config = private$..config, model = private$..model)
      return(model)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Method                                #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$mknBuild(self)
    }
  )
)
