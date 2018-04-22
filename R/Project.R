#' Project
#'
#' \code{Project} Container class for all data relating to the project.
#'
#'
#' @section Core Methods:
#'  \itemize{
#'   \item{\code{new(name = NULL)}}{Initializes an object of the Project class.}
#'   \item{\code{text(x, note = NULL)}}{Method for obtaining/adding/updating text. If no
#'   parameters are presented, the current text is returned.  Otherwise, the text
#'   is updated with the texts of the character vector 'x'. Sentence, word, token, type,
#'   sentence and word length statistics are also computed and the metadata is updated
#'   accordingly.}
#'   \item{\code{summary()}}{Summarizes the Project object.}
#'  }
#'
#' @param name Character string containing the name for the Project object.
#' @param purpose Character string used to indicate how the document will be used, e.g. 'train', 'test'.
#' @param note Character string containing a comment associated with a call to the
#' text method. The texts of the note variable are written to the Projects
#' log. This is used to track changes to the text, perhaps made during preprocessing.
#' @template metadataParams
#'
#' @return Project object, containing the Project text, the metadata and
#' the methods to manage both.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Project <- R6::R6Class(
  classname = "Project",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Set0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name = NULL) {

      private$loadDependencies()
      private$meta <- Meta$new(x = self, name = name)
      private$logR$log(method = 'initialize',
                       event = "Initialization complete.")
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Document Management                           #
    #-------------------------------------------------------------------------#
    addDocument = function(x) {
      private$attach(x)
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$project(self)
    }
  )
)
