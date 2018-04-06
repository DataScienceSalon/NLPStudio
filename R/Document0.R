#------------------------------------------------------------------------------#
#                               Document0                                      #
#------------------------------------------------------------------------------#
#' Document0
#'
#' \code{Document0} Component class defining base methods for all documents.
#'
#' This composite pattern component class defines members and methods common
#' across all the document related classes, including without limitation,
#' Corpus, TextDocument, TermFreq, POS, NGram, and Tokens objects.
#'
#' @section Document0 methods:
#'  \itemize{
#'   \item{\code{new()}}{Method not implemented for this component class.}
#'   \item{\code{summary()}}{Prints a summary of a Document family class object.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(
    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeId = function(verbose = TRUE) {

      identity <- private$meta$getIdentity()
      if (verbose) {
        cat(paste0("\n\nObject Family: ", identity$family))
        cat(paste0("\nObject Class : ", identity$class))
        cat(paste0("\nObject Id    : ", identity$id))
        cat(paste0("\nObject Name  : ", identity$name))
        cat(paste0("\nPurpose      : ", identity$purpose))
      }
      return(identity)

    },

    summarizeDescriptiveMeta = function(verbose = TRUE) {

      meta <- private$meta$getDescriptive()

      if (length(meta) > 0) {
        if (verbose) {
          metaDf <- as.data.frame(meta, stringsAsFactors = FALSE, row.names = NULL)
          cat("\n\nDescriptive:\n")
          print(metaDf, row.names = FALSE)
        }
        return(meta)
      }
      return(NULL)
    },

    summarizeQuant = function(verbose = TRUE) {
      quant <- private$meta$getQuant()
      if (verbose) {
        if (!is.null(quant)) {
          quantDf <- as.data.frame(quant, stringsAsFactors = FALSE, row.names = NULL)
          cat("\n\nQuantitative:\n")
          print(quantDf, row.names = FALSE)
        }
      }
      return(quant)
    },

    summarizeAdmin = function(verbose = TRUE) {
      admin <- private$meta$getAdmin()
      if (verbose) {
        adminDf <- as.data.frame(admin, stringsAsFactors = FALSE, row.names = NULL)
        cat("\nAdministrative:\n")
        print(adminDf, row.names = FALSE)
        cat("\n")
      }
      return(admin)
    },

    summarizeTech = function(verbose = TRUE) {
      tech <- private$meta$getTech()
      if (verbose) {
        techDf <- as.data.frame(tech, stringsAsFactors = FALSE, row.names = NULL)
        cat("\nTechnical:\n")
        print(techDf, row.names = FALSE)
        cat("\n")
      }
      return(tech)
    },

    summarizeDocuments = function(verbose = TRUE) {

      summaries <- list()
      meta <- list()
      familySummary <- data.frame()

      families <- unique(private$..inventory$family)

      for (i in 1:length(families)) {
        documents <- subset(private$..inventory, family == families[i])

        # Get metadata for each document
        for (j in 1:nrow(documents)) {
          id <- documents$id[j]
          document <- private$..documents[[id]]
          meta[[id]] <- document$metadata()
        }

        # Extract identity information
        identity <- rbindlist(lapply(meta, function(m) {
          m$identity
        }))

        # Extract descriptive metadata
        descriptive <- rbindlist(lapply(meta, function(m) {
          m$descriptive
        }), fill = TRUE, use.names = TRUE)

        # Extract quant
        quant <- rbindlist(lapply(meta, function(m) {
          m$quant
        }))

        # Combine and format columns
        if (nrow(descriptive) > 0) {
          familySummary <- cbind(identity, descriptive, quant)
        } else {
          familySummary <- cbind(identity, quant)
        }

        familySummary[is.na(familySummary)] <- " "

        # Print Results if verbose
        if (verbose) {
          cat(paste0("\n\n", families[i], ":\n"))
          print(familySummary[,-1], row.names = FALSE)
        }

        summaries <- c(summaries, familySummary)
      }
      return(summaries)
    }
  ),



  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},

    #-------------------------------------------------------------------------#
    #                 Convenience Getters and Query Method                    #
    #-------------------------------------------------------------------------#
    getId = function() private$meta$getIdentity(key = 'id'),
    getName = function() private$meta$getIdentity(key = 'name'),
    getIdentity = function() private$meta$getIdentity(),
    query = function(key, value) private$meta$query(key, value),

    #-------------------------------------------------------------------------#
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    metadata = function(key = NULL, value = NULL) {
      if (is.null(key)) {
        return(private$meta$getMeta())
      } else {
        private$meta$setDescriptive(key, value)
        invisible(self)
      }
    },

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summary = function(id = TRUE, descriptive = TRUE, quant = TRUE,
                       documents = TRUE, admin = TRUE, tech = TRUE,
                       verbose = TRUE, abbreviated = FALSE) {

      if (abbreviated == FALSE) {
        sd <- list()
        if (id) sd$id <- private$summarizeId(verbose)
        if (quant) sd$quant <- private$summarizeQuant(verbose)
        if (descriptive) sd$meta  <- private$summarizeDescriptiveMeta(verbose)
        if (documents) {
          if (length(private$..documents) > 0) {
            sd$documents <- private$summarizeDocuments(verbose)
          }
        }
        if (admin) sd$admin <- private$summarizeAdmin(verbose)
        if (tech) sd$tech <- private$summarizeTech(verbose)
        invisible(sd)
      } else {
        invisible(private$meta$summary())
      }
    },

    #-------------------------------------------------------------------------#
    #                            Receive Method                               #
    #-------------------------------------------------------------------------#
    receive = function(x, notice) {
      private$meta$modified(event = notice)
      private$logR$log(x = x, event = notice, level = "Info")
    },

    #-------------------------------------------------------------------------#
    #                           Print Log Method                              #
    #-------------------------------------------------------------------------#
    printLog = function() {
      invisible(private$logR$printLog())
    }
  )
)
