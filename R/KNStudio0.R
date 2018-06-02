#==============================================================================#
#                                   KNStudio0                                  #
#==============================================================================#
#' KNStudio0
#'
#' \code{KNStudio} Abstract class for the Kneser-Ney Studio Classes
#'
#' @param x Corpus object upon which the model will be trained.
#' @param size Numeric indicating the model size in terms of nGrams. Defaults to trigram model.
#' @param open Logical indicating whether the vocabulary is open
#' or closed. Open indicates that it is possible to encounter out of vocabulary
#' words in the test set. If TRUE, OOV processing will be performed on
#' the training set. Default is TRUE.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Language Model Classes
#' @family Kneser Ney Model Classes
#' @export
KNStudio0 <- R6::R6Class(
  classname = "KNStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = LMStudio0,

  public = list(
    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function()  { stop("This method is not implemented for this abstract class.") },
    build = function()  { stop("This method is not implemented for this abstract class.") }
  )
)
