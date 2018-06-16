#------------------------------------------------------------------------------#
#                               Primitive0                                     #
#------------------------------------------------------------------------------#
#' Primitive0
#'
#' \code{Primitive0} Abstract class for all primative classes.
#'
#' Abstract class which defines the methods common to all primatives
#' (and by inheritence, composite) objects.
#'
#' @section Primitive0 methods:
#'  \itemize{
#'   \item{\code{new()}}{Method not implemented for this component class.}
#'   \item{\code{summary()}}{Prints a summary of a Document family class object.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document Classes
#' @export
Primitive0 <- R6::R6Class(
  classname = "Primitive0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Super,

  private = list(

    #-------------------------------------------------------------------------#
    #                           Summary Methods                               #
    #-------------------------------------------------------------------------#
    summarizeIdMeta = function() {

      identity <- private$meta$get(type = 'identity')
      NLPStudio::printHeading(text = paste0(identity$classname, ": ",
                                            self$getName(), " Summary"),
                              symbol = "=",
                              newlines = 2)
      cat(paste0("\nObject Class : ", identity$classname))
      cat(paste0("\nObject Id    : ", identity$id))
      cat(paste0("\nObject Name  : ", identity$name))
      return(identity)

    },

    summarizeDescriptiveMeta = function() {
      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())

      descriptive <- private$meta$get(type = 'descriptive')
      if (length(descriptive) > 0) {
        NLPStudio::printHeading(text = paste0(id, " Descriptive Metadata"), symbol = "-")
        metaDf <- as.data.frame(descriptive, stringsAsFactors = FALSE,
                                row.names = NULL)
        print(metaDf, row.names = FALSE)
        return(descriptive)
      }
      return(NULL)
    },

    summarizeQuantMeta = function() {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())

      quant <- private$meta$get(type = 'quant')
      if (length(quant > 0)) {
        NLPStudio::printHeading(text = paste0(id, " Quantitative Metadata"), symbol = "-")
        quantDf <- as.data.frame(quant, stringsAsFactors = FALSE,
                                 row.names = NULL)
        print(quantDf, row.names = FALSE)
        return(quant)
      }
      return(NULL)
    },

    summarizeFunctionalMeta = function() {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())

      functional <- private$meta$get(type = 'functional')
      if (length(functional) > 0) {
        NLPStudio::printHeading(text = paste0(id, " Functional Metadata"), symbol = "-")
        metaDf <- as.data.frame(functional, stringsAsFactors = FALSE,
                                row.names = NULL)
        print(metaDf, row.names = FALSE)
        return(functional)
      }
      return(NULL)
    },


    summarizeAdminMeta = function() {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())

      NLPStudio::printHeading(text = paste0(id, " Administrative Metadata"), symbol = "-")
      admin <- private$meta$get(type = 'admin')
      adminDf <- as.data.frame(admin, stringsAsFactors = FALSE, row.names = NULL)
      if (admin$nModified == 0) {
        adminDf <- adminDf[, !names(adminDf) %in% c("modified", "nModified", "modifiedBy")]
      }
      print(adminDf, row.names = FALSE)
      return(admin)
    },

    summarizeTechMeta = function() {

      identity <- private$meta$get(type = 'identity')
      id <- paste0(identity$classname, ": ", self$getName())

      tech <- private$meta$get(type = 'tech')
      NLPStudio::printHeading(text = paste0(id, " Technical Metadata"), symbol = "-")
      techDf <- as.data.frame(tech, stringsAsFactors = FALSE, row.names = NULL)
      print(techDf, row.names = FALSE)
      return(tech)
    }
  ),

  active = list(

    name = function(value) {
      if (missing(value)) {
        return(private$meta$get(key = 'name'))
      } else {

        # Validate class and length of value parameter.
        private$..params <- list()
        private$..params$classes$name <- list('value')
        private$..params$classes$objects <- list(value)
        private$..params$classes$valid <- list('character')
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'name', event = v$msg, level = "Error")
          stop()
        }
        if (length(value) != 1) {
          event <- paste0("The value parameter for the 'name' method ",
                          "must be character a single string. See ?Primitive0 ",
                          "for further assistance.")
          private$logR$log(method = 'name', event = v$msg, level = "Error")
          stop()
        }
        private$meta$set(key = 'name', value = value, type = 'i')
        event <- paste0("Object name set to '", value, "' .")
        invisible(self)
      }
    },

    description = function(value) {
      if (missing(value)) {
        return(private$meta$get(key = 'description'))
      } else {

        # Validate class and length of value parameter.
        private$..params <- list()
        private$..params$classes$name <- list('value')
        private$..params$classes$objects <- list(value)
        private$..params$classes$valid <- list('character')
        v <- private$validator$validate(self)
        if (v$code == FALSE) {
          private$logR$log(method = 'description', event = v$msg, level = "Error")
          stop()
        }
        if (length(value) != 1) {
          event <- paste0("The value parameter for the 'description' method ",
                          "must be character a single string. See ?Primitive0 ",
                          "for further assistance.")
          private$logR$log(method = 'description', event = v$msg, level = "Error")
          stop()
        }
        private$meta$set(key = 'description', value = value, type = 'descriptive')
        event <- paste0("Object description set to '", value, "' .")
        invisible(self)
      }
    }
  ),



  public = list(
    initialize = function() {stop("This method is not implemented for this abstract class ")},

    #-------------------------------------------------------------------------#
    #                             Metadata Methods                            #
    #-------------------------------------------------------------------------#
    getId = function() { return(private$meta$get(key = 'id')) },

    getName = function() { return(private$meta$get(key = 'name')) },
    setName = function(name) { return(private$meta$set(key = "name",
                                                       value = name,
                                                       type = 'i'))},

    getMeta = function(key = NULL, type = NULL)  private$meta$get(key = key, type = type),
    setMeta = function(key, value, type = 'd') {
      private$meta$set(key = key, value = value, type = type)
      invisible(self)
    },
    getQuant = function() private$meta$get(type = 'q'),
    setQuant = function(quant) {
      private$meta$set(key = names(quant), value = unlist(quant), type = 'q')
    },

    query = function(key, value) { return(private$meta$query(key, value)) },

    #-------------------------------------------------------------------------#
    #                            Message Method                               #
    #-------------------------------------------------------------------------#
    message = function(event) {
      private$meta$modified(event = event)
      private$logR$log(method = 'message', event = event, level = "Info")
    },

    #-------------------------------------------------------------------------#
    #                           Summary Method                                #
    #-------------------------------------------------------------------------#
    summary = function(section = NULL) {

      sd <- list()

      if (!is.null(section)) {
        if (grepl("^i", section, ignore.case = TRUE)) sd$id <- private$summarizeIdMeta()
        if (grepl("^d", section, ignore.case = TRUE)) sd$descriptive <- private$summarizeDescriptiveMeta()
        if (grepl("^q", section, ignore.case = TRUE)) sd$quant <- private$summarizeQuantMeta()
        if (grepl("^f", section, ignore.case = TRUE)) sd$functional  <- private$summarizeFunctionalMeta()
        if (grepl("^a", section, ignore.case = TRUE)) sd$admin <- private$summarizeAdminMeta()
        if (grepl("^t", section, ignore.case = TRUE)) sd$tech <- private$summarizeTechMeta()

      } else {

        sd$id <- private$summarizeIdMeta()
        sd$descriptive <- private$summarizeDescriptiveMeta()
        sd$quant <- private$summarizeQuantMeta()
        sd$functional  <- private$summarizeFunctionalMeta()
        sd$admin <- private$summarizeAdminMeta()
        sd$tech <- private$summarizeTechMeta()
      }
      return(sd)
    },

    #-------------------------------------------------------------------------#
    #                           Print Log Method                              #
    #-------------------------------------------------------------------------#
    printLog = function() {
      invisible(private$logR$printLog())
    }
  )
)
