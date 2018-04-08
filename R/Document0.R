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
    summarizeIdMeta = function() {

      identity <- private$meta$getIdentity()
      cat(paste0("\nObject Class : ", identity$classname))
      cat(paste0("\nObject Id    : ", identity$id))
      return(identity)

    },

    summarizeDescriptiveMeta = function() {

      descriptive <- private$meta$getDescriptive()
      if (length(descriptive) > 0) {
        metaDf <- as.data.frame(descriptive, stringsAsFactors = FALSE,
                                row.names = NULL)
        cat("\n\nDescriptive:\n")
        print(metaDf, row.names = FALSE)
        return(descriptive)
      }
      return(NULL)
    },

    summarizeQuantMeta = function() {

      quant <- private$meta$getQuant()
      if (length(quant > 0)) {
        quantDf <- as.data.frame(quant, stringsAsFactors = FALSE,
                                 row.names = NULL)
        cat("\n\nQuantitative:\n")
        print(quantDf, row.names = FALSE)
        return(quant)
      }
      return(NULL)
    },

    summarizeFunctionalMeta = function() {

      functional <- private$meta$getFunctional()
      if (length(functional) > 0) {
        metaDf <- as.data.frame(functional, stringsAsFactors = FALSE,
                                row.names = NULL)
        cat("\n\nFunctional:\n")
        print(metaDf, row.names = FALSE)
        return(functional)
      }
      return(NULL)
    },


    summarizeAdminMeta = function() {

      admin <- private$meta$getAdmin()
      adminDf <- as.data.frame(admin, stringsAsFactors = FALSE, row.names = NULL)
      cat("\nAdministrative:\n")
      print(adminDf, row.names = FALSE)
      cat("\n")
      return(admin)
    },

    summarizeTechMeta = function() {

      tech <- private$meta$getTech()

      techDf <- as.data.frame(tech, stringsAsFactors = FALSE, row.names = NULL)
      cat("\nTechnical:\n")
      print(techDf, row.names = FALSE)
      cat("\n")
      return(tech)
    }
  ),



  public = list(
    initialize = function() {stop("This method is not implemented for this component class ")},

    #-------------------------------------------------------------------------#
    #                             Metadata Methods                            #
    #-------------------------------------------------------------------------#
    getId = function() { return(private$meta$getIdentity(key = 'id')) },
    getName = function() { return(private$meta$getIdentity(key = 'name')) },
    getIdentity = function() { return(private$meta$getIdentity()) },
    getMeta = function() { return(private$meta$getMeta()) },
    getDescriptiveMeta = function(key = NULL) { return(private$meta$getDescriptive(key)) },
    getQuantMeta = function() { return(private$meta$getQuant()) },
    getFunctionalMeta = function() { return(private$meta$getFunctional()) },
    getAdminMeta = function() { return(private$meta$getAdmin()) },
    getTechMeta = function() { return(private$meta$getTech()) },
    query = function(key, value) { return(private$meta$query(key, value)) },

    setDescriptiveMeta = function(key, value) {
      private$meta$setDescriptive(key, value)
      invisible(self)
    },

    setFunctionalMeta = function(key, value) {
      private$meta$setFunctional(key, value)
      invisible(self)
    },

    setTechMeta = function(key, value) {
      private$meta$setTech(key, value)
      invisible(self)
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
