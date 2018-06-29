#------------------------------------------------------------------------------#
#                                   Composite0                                 #
#------------------------------------------------------------------------------#
#' Composite0
#'
#' \code{Composite0} Abstract class from which all composite classes are derived.
#'
#' This abstract class defines members and methods common to the composite classes.
#'
#' @section Composite0 methods:
#'  \itemize{
#'   \item{\code{new()}}{Not implemented.}
#'   \item{\code{getChildren(key, value)}}{Returns the child or children that
#'   match the key/value pair(s). }
#'   \item{\code{addChild(x)}}{Adds a child to the Set object.}
#'   \item{\code{removeChild(x)}}{Removes a child from the Set
#'   object.}
#'   \item{\code{summary(x)}}{Summarizes a Set child.}
#'  }
#'
#'  @param x Object or list of objects to be attached
#'  @param key Character string or vector of strings indicating the metadata
#'  variable or variables used for matching
#'  @param value Character string or vector of strings indicating the
#'  metadata value associated with the key(s) parameter.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Child Classes
#' @export
Composite0 <- R6::R6Class(
  classname = "Composite0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Primitive0,

  private = list(
    ..children = list(),
    ..inventory = data.frame(),

    #-------------------------------------------------------------------------#
    #                             Search Method                               #
    #-------------------------------------------------------------------------#
    search = function(key, value) {

      listCondition <- rep(FALSE, length(private$..children))

      if (!is.null(private$..children)) {
        listCondition <- sapply(private$..children, function(a) {
          a$query(key = key, value = value)
        })
      }

      return(listCondition)
    },

    #-------------------------------------------------------------------------#
    #                             attach Child                                #
    #-------------------------------------------------------------------------#
    attach = function(x) {

      # Add Child to inventory
      credentials <- x$getMeta(type = 'identity')
      private$..children[[credentials$id]] <- x

      # Update inventory data frame
      private$..inventory <- private$..inventory %>% filter(id != credentials$id)
      credentials <- as.data.frame(credentials, stringsAsFactors = FALSE, row.names = NULL)
      private$..inventory <- rbind(private$..inventory, credentials)

      # Update date/time metadata and create log entry
      classname <- x$getMeta(key = "classname")
      event <- paste0("Added ", classname, " object '", x$getName(), "' to ", self$getName(), ".")
      private$meta$modified(event = event)
      private$logR$log(method = 'addChild', event = event)
    },

    detach = function(x) {

      identifier <- x$getId()

      if (!is.null(private$..children[[identifier]])) {
        private$..children[[identifier]] <- NULL
        private$..inventory <- private$..inventory %>%
          dplyr::filter(id != identifier)
        event <- paste0("Removed ", x$getName(), " from ",
                        self$getName(), ".")
        private$meta$modified(event = event)
        private$logR$log(method = 'detach',
                         event = event)
      } else {
        event <- paste0("Object is not attached to ",
                        self$getName(), ". ")
        private$logR$log( method = 'detach',
                          event = event, level = "Warn")
      }
    },

    #-------------------------------------------------------------------------#
    #                     Summary Children Method                             #
    #-------------------------------------------------------------------------#
    summarizeChildMeta = function(classname = NULL) {

      if (length(private$..children) == 0) return(NULL)

      heading <- paste0("Objects attached to ", self$getName())
      NLPStudio::printHeading(text = heading, symbol = "-")
      childMeta <- private$getChildMeta(classname)
      classes <- names(childMeta)
      lapply(seq_along(childMeta), function(x) {

        # Combine relevant metadata types
        sections <- list(childMeta[[x]]$identity,
                         childMeta[[x]]$descriptive,
                         childMeta[[x]]$quant,
                         childMeta[[x]]$functional,
                         childMeta[[x]]$admin,
                         childMeta[[x]]$tech)

        # Combine data into a single data frame
        childSummary <- sections[[1]]
        for (i in 2:length(sections)) {
         if (nrow(sections[[i]]) > 0)  childSummary <- cbind(childSummary, sections[[i]])
        }

        #Remove non-essential data, replace NAs with spaces and print
        childSummary <- childSummary %>% select(-modified, -modifiedBy, -nModified,
                                            -lastState, -hardware, -os,
                                            -release, -version)
        childSummary[is.na(childSummary)] <- " "
        cat("\n")
        print(childSummary, row.names = FALSE)
      })

      return(childMeta)
    },


    #-------------------------------------------------------------------------#
    #                           Child Metadata                                #
    #-------------------------------------------------------------------------#
    getChildMeta = function(classname = NULL, type = NULL) {

      if (nrow(private$..inventory) == 0) return(NULL)

      classes <- unique(private$..inventory$classname)

      if (!is.null(classname)) {
        if (classname %in% classes) {
          classes <- classname
        } else {
          event <- paste0("No children of the ", classname, " class are ",
                          "attached to this object.")
          private$logR$log(x = self, event = event, method = "getChildMeta",
                           level = "Error")
          stop()
        }
      }

      childMeta <- list()


      for (i in 1:length(classes)) {
        children <- subset(private$..inventory, classname == classes[i])

        # Get metadata for each child
        meta <- list()
        for (j in 1:nrow(children)) {
          id <- children$id[j]
          child <- private$..children[[id]]
          meta[[id]] <- child$getMeta()
        }

        section <- list()

        # Extract identity information
        section$identity <- rbindlist(lapply(meta, function(m) {
          m$identity
        }))

        # Extract descriptive metadata
        section$descriptive <- as.data.frame(rbindlist(lapply(meta, function(m) {
          m$descriptive
        }), fill = TRUE, use.names = TRUE))
        section$descriptive[is.na(section$descriptive)] <- " "

        # Extract functional metadata
        section$functional <- as.data.frame(rbindlist(lapply(meta, function(m) {
          m$functional
        }), fill = TRUE, use.names = TRUE))
        section$functional[is.na(section$functional)] <- " "

        # Extract quant metadata
        section$quant <- rbindlist(lapply(meta, function(m) {
          m$quant
        }))

        # Extract admin metadata
        section$admin <- rbindlist(lapply(meta, function(m) {
          m$admin
        }))

        # Extract technical metadata
        section$tech <- rbindlist(lapply(meta, function(m) {
          m$tech
        }))

        if (!is.null(type)) {

          if (grepl("^i", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$identity
          } else if (grepl("^q", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$quant
          } else if (grepl("^d", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$descriptive
          } else if (grepl("^f", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$functional
          } else if (grepl("^a", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$admin
          } else if (grepl("^t", type, ignore.case = TRUE)) {
            childMeta[[classes[i]]] <- section$tech
          }
        } else {
          childMeta[[classes[i]]] <- section
        }
      }
      return(childMeta)
    },
    #-------------------------------------------------------------------------#
    setChildMeta = function(childMeta, classname = "Primitive0",
                            type = 'descriptive') {

      # Validate class of object.
      private$..params <- list()
      private$..params$classes$name <- list('childMeta')
      private$..params$classes$objects <- list(childMeta)
      private$..params$classes$valid <- list(c('data.frame', 'data.table'))
      v <- private$validator$validate(self)
      if (v$code == FALSE) {
        private$logR$log(method = 'setChildMeta',
                         event = v$msg, level = "Error")
        stop()
      }

      cls <- classname
      if (nrow(private$..inventory %>% filter(classname == cls)) == 0) {
        event <- paste0("There are no children of type ", classname,
                        " attached to this collection. See?",
                        class(self)[1], " for further assistance.")
        private$logR$log(method = 'setChildMeta', event = event,
                         level = "Error")
        stop()
      }


      if (nrow(childMeta) != length(private$..children)) {
        event <- paste0("The childMeta variable must be a data.frame or ",
                        "data.table with one row for each child in ",
                        "the collection. See?", class(self)[1], " for ",
                        "further assistance.")
        private$logR$log(method = "setChildMeta", event = event, level = "Error")
      }

      vars <- names(childMeta)
      for (i in 1:nrow(childMeta)) {
        for (j in 1:length(childMeta))
          private$..children[[i]]$setMeta(key = vars[j], value = childMeta[i,j], type = type)
      }

      return(self)
    }
  ),

  public = list(
    initialize = function() {stop("This method not implemented for this abstract class.")},

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
        if (grepl("^c", section, ignore.case = TRUE)) sd$children  <- private$summarizeChildlMeta()
        if (grepl("^a", section, ignore.case = TRUE)) sd$admin <- private$summarizeAdminMeta()
        if (grepl("^t", section, ignore.case = TRUE)) sd$tech <- private$summarizeTechMeta()

      } else {

        sd$id <- private$summarizeIdMeta()
        sd$descriptive <- private$summarizeDescriptiveMeta()
        sd$quant <- private$summarizeQuantMeta()
        sd$functional  <- private$summarizeFunctionalMeta()
        sd$children <- private$summarizeChildMeta()
        sd$admin <- private$summarizeAdminMeta()
        sd$tech <- private$summarizeTechMeta()
      }
      invisible(sd)
    }
  )
)
